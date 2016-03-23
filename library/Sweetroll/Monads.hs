{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad that contains the application context + some
-- functions that use it, such as HTTP client requests.
module Sweetroll.Monads where

import           Sweetroll.Prelude
import           Control.Monad.Base
import           Control.Monad.Reader hiding (forM_)
import           Control.Monad.Except hiding (forM_)
import           System.Directory
import           System.Process (readProcessWithExitCode)
import           System.FilePath.Posix
import           System.IO.Unsafe
import           Data.Pool
import           Data.Maybe
import           GHC.Conc (getNumCapabilities)
import           Network.HTTP.Client.Conduit
import           Servant
import           Gitson
import           Data.FileEmbed
import           Scripting.Duktape
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxDeleted  ∷ TVar [String]
  , _ctxTplPool  ∷ Pool DuktapeCtx
  , _ctxLock     ∷ MVar ()
  , _ctxHttpMgr  ∷ Manager }

type MonadSweetroll = MonadReader SweetrollCtx

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

instance HasGitsonLock Sweetroll where
  getGitsonLock = asks _ctxLock

newtype Sweetroll α = Sweetroll {
  runSweetroll ∷ ReaderT SweetrollCtx (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadError ServantErr,
            MonadSweetroll)

instance MonadBaseControl IO Sweetroll where
  type StM Sweetroll α = StM (ReaderT SweetrollCtx (ExceptT ServantErr IO)) α
  liftBaseWith f = Sweetroll $ liftBaseWith $ \x → f $ x . runSweetroll
  restoreM       = Sweetroll . restoreM

runSweetrollEither ∷ SweetrollCtx → Sweetroll α → EitherT ServantErr IO α
runSweetrollEither ctx sweet = EitherT $ liftIO $ runExceptT $ runReaderT (runSweetroll sweet) ctx

sweetrollToEither ∷ SweetrollCtx → Sweetroll :~> EitherT ServantErr IO
sweetrollToEither ctx = Nat $ runSweetrollEither ctx

initCtx ∷ SweetrollConf → SweetrollSecrets → IO SweetrollCtx
initCtx conf secs = do
  hmg ← newManager
  lck ← newMVar ()
  cpus ← getNumCapabilities
  --        static pool, basically: max Ncpus, don't expire
  tplPool ← createPool createTemplateCtx (\_ → return ()) 1 999999999999 cpus
  (_, deleted', _) ← readProcessWithExitCode "git" [ "log", "--all", "--diff-filter=D", "--find-renames", "--name-only", "--pretty=format:" ] ""
  deleted ← newTVarIO $ lines deleted'
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxSecs     = secs
                      , _ctxDeleted  = deleted
                      , _ctxTplPool  = tplPool
                      , _ctxLock     = lck
                      , _ctxHttpMgr  = hmg }

createTemplateCtx ∷ IO DuktapeCtx
createTemplateCtx = do
  duk ← fromJust <$> createDuktapeCtx
  void $ evalDuktape duk "var window = this"
  void $ evalDuktape duk $(embedFile "bower_components/lodash/dist/lodash.min.js")
  void $ evalDuktape duk $(embedFile "bower_components/moment/min/moment-with-locales.min.js")
  void $ evalDuktape duk $(embedFile "bower_components/SparkMD5/spark-md5.min.js")
  void $ evalDuktape duk $(embedFile "templates/prelude.js")
  let setTpl tname tcontent =
        void $ if takeExtension tname == ".js"
                 then evalDuktape duk tcontent
                 else callDuktape duk Nothing "setTemplate" [ String $ cs $ dropExtensions tname, String $ cs tcontent ]
      notJs = (/= ".js") . takeExtension -- Sort to evaluate JS first, important for prelude.js to define SweetrollTemplates
  forM_ (sortOn (notJs . fst) $(embedDir "templates")) $ uncurry setTpl
  hasUserTpls ← doesDirectoryExist "templates"
  when hasUserTpls $ do
    userTpls ← getDirectoryContents "templates"
    forM_ (sortOn notJs $ filter (not . ("." `isPrefixOf`)) userTpls) $ \tname → do -- Avoid ., .., .DS_Store and all hidden files really
      thtml ← (try $ readFile $ "templates" </> tname) ∷ IO (Either IOException ByteString)
      case thtml of
        Right h → setTpl tname h
        Left e → void $ putStrLn $ "Error when reading template " ++ cs tname ++ ": " ++ cs (show e)
  return duk

getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → Maybe α) → μ α
getConfOpt f = asks $ fromMaybe (fromJust $ f def) . f . _ctxConf

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getDeleted ∷ MonadSweetroll μ ⇒ μ (TVar [String])
getDeleted = asks _ctxDeleted

getRenderer ∷ MonadSweetroll μ ⇒ μ (ByteString → Value → Text)
getRenderer = renderer <$> asks _ctxTplPool
  where renderer p x y = txtVal $ unsafePerformIO $ withResource p $ \duk → callDuktape duk (Just "SweetrollTemplates") x [y]
        {-# NOINLINE renderer #-}
        txtVal (Right (Just (String t))) = t
        txtVal (Right (Just _)) = "TEMPLATE ERROR: returned something other than a string"
        txtVal (Right Nothing) = "TEMPLATE ERROR: returned nothing"
        txtVal (Left e) = "TEMPLATE ERROR: " ++ cs e

parseEntryURI ∷ (MonadError ServantErr μ, MonadSweetroll μ) ⇒
                URI → μ (String, String)
parseEntryURI uri = do
  base ← getConfOpt baseURI
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority uri) == (uriRegName <$> uriAuthority base)
  parseEntryURIRelative uri
