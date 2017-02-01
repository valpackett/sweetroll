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
import           Sweetroll.HTTPClient (jsonFetch)
import           System.Process (readProcessWithExitCode)
import           Data.Maybe
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
  , _ctxPlugCtx  ∷ DuktapeCtx
  , _ctxLock     ∷ MVar ()
  , _ctxHttpMgr  ∷ Manager }

type MonadSweetroll = MonadReader SweetrollCtx

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

instance HasGitsonLock Sweetroll where
  getGitsonLock = asks _ctxLock

newtype Sweetroll α = Sweetroll {
  runSweetroll ∷ ReaderT SweetrollCtx (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadCatch, MonadError ServantErr,
            MonadSweetroll)

instance MonadBaseControl IO Sweetroll where
  type StM Sweetroll α = StM (ReaderT SweetrollCtx (ExceptT ServantErr IO)) α
  liftBaseWith f = Sweetroll $ liftBaseWith $ \x → f $ x . runSweetroll
  restoreM       = Sweetroll . restoreM

runSweetrollExcept ∷ SweetrollCtx → Sweetroll α → ExceptT ServantErr IO α
runSweetrollExcept ctx sweet = ExceptT $ liftIO $ runExceptT $ runReaderT (runSweetroll sweet) ctx

sweetrollToExcept ∷ SweetrollCtx → Sweetroll :~> ExceptT ServantErr IO
sweetrollToExcept ctx = Nat $ runSweetrollExcept ctx

initCtx ∷ SweetrollConf → SweetrollSecrets → IO SweetrollCtx
initCtx conf secs = do
  hmg ← newManager
  lck ← newMVar ()
  cpus ← getNumCapabilities
  --        static pool, basically: max Ncpus, don't expire
  (_, deleted', _) ← readProcessWithExitCode "git" [ "log", "--all", "--diff-filter=D", "--find-renames", "--name-only", "--pretty=format:" ] ""
  plugCtx ← createPluginsCtx conf hmg
  deleted ← newTVarIO $ lines deleted'
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxSecs     = secs
                      , _ctxDeleted  = deleted
                      , _ctxPlugCtx  = plugCtx
                      , _ctxLock     = lck
                      , _ctxHttpMgr  = hmg }

createPluginsCtx ∷ SweetrollConf → Manager → IO DuktapeCtx
createPluginsCtx conf hmg = do
  duk ← fromJust <$> createDuktapeCtx
  void $ evalDuktape duk "var window = this"
  void $ evalDuktape duk $(embedFile "bower_components/lodash/dist/lodash.min.js")
  void $ evalDuktape duk $(embedFile "bower_components/moment/min/moment-with-locales.min.js")
  void $ evalDuktape duk $(embedFile "library/Sweetroll/PluginApi.js")
  void $ callDuktape duk (Just "Sweetroll") "_setConf" [ toJSON conf ]
  void $ exposeFnDuktape duk (Just "Sweetroll") "fetch" $ jsonFetch hmg
  forFileIn "plugins" (filter (not . ("." `isPrefixOf`))) (\_ x → void $ evalDuktape duk x)
  return duk


getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → Maybe α) → μ α
getConfOpt f = asks $ fromMaybe (fromJust $ f def) . f . _ctxConf

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getDeleted ∷ MonadSweetroll μ ⇒ μ (TVar [String])
getDeleted = asks _ctxDeleted

runCategoryDeciders ∷ (MonadIO μ, MonadSweetroll μ) ⇒ Value → μ Text
runCategoryDeciders v = do
  duk ← asks _ctxPlugCtx
  result ← callDuktape duk (Just "Sweetroll") "_runCategoryDeciders" [ v ]
  putStrLn $ "Category decider result: " ++ tshow result
  return $ fromMaybe "notes" $ (^? key "name" . _String) =<< join (hush result)

notifyPlugins ∷ (MonadIO μ, MonadSweetroll μ) ⇒ Text → Value → μ ()
notifyPlugins ename edata = do
  duk ← asks _ctxPlugCtx
  void $ callDuktape duk (Just "Sweetroll") "_fireEvent" [ String ename, edata ]

parseEntryURI ∷ (MonadError ServantErr μ, MonadSweetroll μ) ⇒
                URI → μ (String, String)
parseEntryURI uri = do
  base ← getConfOpt baseURI
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority uri) == (uriRegName <$> uriAuthority base)
  parseEntryURIRelative uri
