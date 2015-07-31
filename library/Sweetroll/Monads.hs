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
import qualified Control.Exception.Lifted as E
import           Data.Stringable hiding (length)
import           Data.Aeson.Types
import           Network.HTTP.Client
import qualified Network.HTTP.Client.Conduit as H
import qualified Network.HTTP.Types as HT
import "crypto-random" Crypto.Random
import           Servant hiding (toText)
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxTpls     ∷ SweetrollTemplates
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxHostInfo ∷ [Pair]
  , _ctxHttpMgr  ∷ H.Manager
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
  httpClientMgr ← H.newManager
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

instance H.HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

requestGetHtml ∷ (Stringable α, Stringable β) ⇒ β → Sweetroll (Maybe α)
requestGetHtml url = do
  req ← parseUrlP "" $ toString url
  resp' ← E.try $ request $ req { requestHeaders = [ (HT.hAccept, "text/html; charset=utf-8") ] } ∷ Sweetroll (Either SomeException (Response LByteString))
  case resp' of
    Left _ → return Nothing
    Right resp → do
      putStrLn $ "HTML fetching status for " ++ toText url ++  ": " ++ (toText . show . HT.statusCode . responseStatus $ resp)
      return $ case HT.statusCode $ responseStatus $ resp of
        200 → Just $ fromLazyByteString $ responseBody resp
        _ → Nothing

request ∷ (MonadSweetroll μ, MonadIO μ, Stringable α) ⇒ H.Request → μ (H.Response α)
request req = do
  resp ← H.httpLbs req
  return $ resp { H.responseBody = fromLazyByteString $ H.responseBody resp }

parseUrlP ∷ (MonadIO μ) ⇒ String → String → μ H.Request
parseUrlP postfix url = liftIO . H.parseUrl $ url ++ postfix
