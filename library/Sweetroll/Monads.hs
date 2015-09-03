{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE PackageImports, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad that contains the application context + some
-- functions that use it, such as HTTP client requests.
module Sweetroll.Monads where

import           ClassyPrelude
import           Control.Monad.Base
import           Control.Monad.Reader hiding (forM_)
import           Control.Monad.Except hiding (forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Unsafe
import           Data.Conduit
import           Data.Maybe
import           Data.String.Conversions
import           Data.Aeson.Types
import           Text.RawString.QQ
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import           Servant
import           Scripting.Duktape
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxDuk      ∷ DuktapeCtx
  , _ctxHttpMgr  ∷ Manager }

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

type MonadSweetroll = MonadReader SweetrollCtx

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
  httpClientMgr ← newManager
  duk ← createDuktapeCtx
  loadTemplates $ fromJust duk
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxSecs     = secs
                      , _ctxDuk      = fromJust duk
                      , _ctxHttpMgr  = httpClientMgr }

loadTemplates ∷ DuktapeCtx → IO ()
loadTemplates duk = do
  -- TODO: compile time check that bower deps have been fetched!
  void $ evalDuktape duk $ fromJust $ lookup "lodash/lodash.min.js" bowerComponents
  void $ evalDuktape duk $ fromJust $ lookup "moment/min/moment-with-locales.min.js" bowerComponents
  void $ evalDuktape duk [r|
    var SweetrollTemplates = {}
    setTemplate = function (name, html) {
      SweetrollTemplates[name] = _.template(html, {
        'sourceURL': name, 'variable': 'scope',
        'imports': { 'templates': SweetrollTemplates }
      })
    }|]
  let setTpl tname thtml = void $ callDuktape duk Nothing "setTemplate" [ String $ cs $ dropExtensions tname, String $ cs thtml ]
  forM_ defaultTemplates $ uncurry setTpl
  hasUserTpls ← doesDirectoryExist "templates"
  when hasUserTpls $ do
    userTpls ← getDirectoryContents "templates"
    forM_ (filter (not . ("." `isPrefixOf`)) userTpls) $ \tname → do -- Avoid ., .., .DS_Store and all hidden files really
      thtml ← (try $ readFile $ "templates" </> tname) ∷ IO (Either IOException ByteString)
      case thtml of
        Right h → setTpl tname h
        Left e → void $ putStrLn $ "Error when reading template " ++ cs tname ++ ": " ++ (cs $ show e)

getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → Maybe α) → μ α
getConfOpt f = asks $ fromMaybe (fromJust $ f def) . f . _ctxConf

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getRenderer ∷ MonadSweetroll μ ⇒ μ (ByteString → Value → Text)
getRenderer = liftM renderer $ asks _ctxDuk
  where renderer duk x y = txtVal $ unsafePerformIO $ callDuktape duk (Just "SweetrollTemplates") x [y]
        {-# NOINLINE renderer #-}
        txtVal (Right (Just (String t))) = t
        txtVal (Right (Just _)) = "TEMPLATE ERROR: returned something other than a string"
        txtVal (Right Nothing) = "TEMPLATE ERROR: returned nothing"
        txtVal (Left e) = "TEMPLATE ERROR: " ++ cs e

request ∷ ConvertibleStrings LByteString α ⇒ Request → Sweetroll (Response α)
request req = do
  resp ← httpLbs req
  return $ resp { responseBody = cs $ responseBody resp }

withSuccessfulRequest ∷ Request → (Response (Source Sweetroll ByteString) → Sweetroll (Maybe α)) → Sweetroll (Maybe α)
withSuccessfulRequest req a =
  withResponse req $ \resp → do
    putStrLn $ cs $ "Request status for <" ++ show (getUri req) ++ ">: " ++ (show . statusCode . responseStatus $ resp)
    if responseStatus resp `elem` [ok200, created201, accepted202, noContent204] then a resp else return Nothing

withSuccessfulRequestHtml ∷ URI → (Response (Source Sweetroll ByteString) → Sweetroll (Maybe α)) → Sweetroll (Maybe α)
withSuccessfulRequestHtml uri a = do
  req ← setUri def uri
  let req' = req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] }
  withSuccessfulRequest req' a

guardJust ∷ ServantErr → Sweetroll (Maybe α) → Sweetroll α
guardJust e a = a >>= \x → case x of
                               Just x' → return x'
                               Nothing → throwError e
