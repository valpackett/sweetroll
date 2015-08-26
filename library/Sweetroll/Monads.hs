{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE PackageImports, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad that contains the application context + some
-- functions that use it, such as HTTP client requests.
module Sweetroll.Monads where

import           ClassyPrelude
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           Data.Conduit
import           Data.Stringable hiding (length)
import           Data.Aeson.Types
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import "crypto-random" Crypto.Random
import           Servant hiding (toText)
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxTpls     ∷ SweetrollTemplates
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxHostInfo ∷ [Pair]
  , _ctxHttpMgr  ∷ Manager
  , _ctxRng      ∷ SystemRNG }

type MonadSweetroll = MonadReader SweetrollCtx

newtype Sweetroll α = Sweetroll {
  runSweetroll ∷ ReaderT SweetrollCtx (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadError ServantErr,
            MonadSweetroll)

instance MonadBaseControl IO Sweetroll where
  type StM Sweetroll α = StM (ReaderT SweetrollCtx (ExceptT ServantErr IO)) α
  liftBaseWith f = Sweetroll $ liftBaseWith $ \r → f $ r . runSweetroll
  restoreM       = Sweetroll . restoreM

runSweetrollEither ∷ SweetrollCtx → Sweetroll α → EitherT ServantErr IO α
runSweetrollEither ctx sweet = EitherT $ liftIO $ runExceptT $ runReaderT (runSweetroll sweet) ctx

sweetrollToEither ∷ SweetrollCtx → Sweetroll :~> EitherT ServantErr IO
sweetrollToEither ctx = Nat $ runSweetrollEither ctx

initCtx ∷ SweetrollConf → SweetrollTemplates → SweetrollSecrets → IO SweetrollCtx
initCtx conf tpls secs = do
  httpClientMgr ← newManager
  sysRandom ← cprgCreate <$> createEntropyPool
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxTpls     = tpls
                      , _ctxSecs     = secs
                      , _ctxHostInfo = [ "domain"   .= domainName conf
                                       , "s"        .= s conf
                                       , "base_url" .= baseUrl conf ]
                      , _ctxHttpMgr  = httpClientMgr
                      , _ctxRng      = sysRandom }

getCtx ∷ MonadSweetroll μ ⇒ μ SweetrollCtx
getCtx = ask

getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → α) → μ α
getConfOpt f = asks $ f . _ctxConf

getTpls ∷ MonadSweetroll μ ⇒ μ SweetrollTemplates
getTpls = asks _ctxTpls

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getSec ∷ MonadSweetroll μ ⇒ (SweetrollSecrets → α) → μ α
getSec f = asks $ f . _ctxSecs

getHostInfo ∷ MonadSweetroll μ ⇒ μ [Pair]
getHostInfo = asks _ctxHostInfo

getRng ∷ MonadSweetroll μ ⇒ μ SystemRNG
getRng = asks _ctxRng

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

-- TODO: delete
parseUrlP ∷ (MonadIO μ) ⇒ String → String → μ Request
parseUrlP postfix url = liftIO . parseUrl $ url ++ postfix

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
