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
import           Data.Stringable hiding (length)
import           Data.Aeson.Types
import           Text.RawString.QQ
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import "crypto-random" Crypto.Random
import           Servant hiding (toText)
import           Scripting.Duktape
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxDuk      ∷ DuktapeCtx
  , _ctxHttpMgr  ∷ Manager
  , _ctxRng      ∷ SystemRNG }

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
  sysRandom ← cprgCreate <$> createEntropyPool
  duk ← createDuktapeCtx
  loadTemplates $ fromJust duk
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxSecs     = secs
                      , _ctxDuk      = fromJust duk
                      , _ctxHttpMgr  = httpClientMgr
                      , _ctxRng      = sysRandom }


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
  let setTpl tname thtml = void $ callDuktape duk Nothing "setTemplate" [ String $ toText $ dropExtensions tname, String $ toText thtml ]
  forM_ defaultTemplates $ uncurry setTpl
  hasUserTpls ← doesDirectoryExist "templates"
  if hasUserTpls
     then do
       userTpls ← getDirectoryContents "templates"
       forM_ (filter (\x → x /= "." && x /= "..") userTpls) $ \tname → do
         thtml ← readFile $ "templates" </> tname
         setTpl tname thtml
      else return ()

getCtx ∷ MonadSweetroll μ ⇒ μ SweetrollCtx
getCtx = ask

getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → α) → μ α
getConfOpt f = asks $ f . _ctxConf

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getSec ∷ MonadSweetroll μ ⇒ (SweetrollSecrets → α) → μ α
getSec f = asks $ f . _ctxSecs

getRenderer ∷ MonadSweetroll μ ⇒ μ (ByteString → Value → Text)
getRenderer = liftM renderer $ asks _ctxDuk
  where renderer duk x y = txtVal $ unsafePerformIO $ callDuktape duk (Just "SweetrollTemplates") x [y]
        {-# NOINLINE renderer #-}
        txtVal (Right (Just (String t))) = t
        txtVal (Right (Just _)) = "TEMPLATE ERROR: returned something other than a string"
        txtVal (Right Nothing) = "TEMPLATE ERROR: returned nothing"
        txtVal (Left e) = "TEMPLATE ERROR: " ++ toText e

getRng ∷ MonadSweetroll μ ⇒ μ SystemRNG
getRng = asks _ctxRng

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

request ∷ Stringable α ⇒ Request → Sweetroll (Response α)
request req = do
  resp ← httpLbs req
  return $ resp { responseBody = fromLazyByteString $ responseBody resp }

withSuccessfulRequest ∷ Request → (Response (Source Sweetroll ByteString) → Sweetroll (Maybe α)) → Sweetroll (Maybe α)
withSuccessfulRequest req a =
  withResponse req $ \resp → do
    putStrLn $ toText $ "Request status for <" ++ show (getUri req) ++ ">: " ++ (show . statusCode . responseStatus $ resp)
    if responseStatus resp `elem` [ok200, created201, accepted202, noContent204] then a resp else return Nothing

withSuccessfulRequestHtml ∷ URI → (Response (Source Sweetroll ByteString) → Sweetroll (Maybe α)) → Sweetroll (Maybe α)
withSuccessfulRequestHtml uri a = do
  req ← setUri def uri
  let req' = req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] }
  withSuccessfulRequest req' a
