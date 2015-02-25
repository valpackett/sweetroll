{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE PackageImports, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

-- | Monads that power Sweetroll and some common basic functions around them.
module Sweetroll.Monads (module Sweetroll.Monads) where

import           ClassyPrelude
import           Data.Stringable hiding (length)
import           Data.Aeson.Types
import           Control.Monad.Reader
import           Network.Wai (Application)
import           Network.HTTP.Types
import qualified Network.HTTP.Client.Conduit as H
import "crypto-random" Crypto.Random
import           Web.Scotty.Trans hiding (request)
import           Sweetroll.Util (writeForm)
import           Sweetroll.Conf

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxTpls     ∷ SweetrollTemplates
  , _ctxHostInfo ∷ [Pair]
  , _ctxHttpMgr  ∷ H.Manager
  , _ctxRng      ∷ SystemRNG }

type MonadSweetroll  = MonadReader SweetrollCtx
type SweetrollBase   = ReaderT SweetrollCtx IO
type SweetrollAction = ActionT LText SweetrollBase
type SweetrollApp    = ScottyT LText SweetrollBase ()

instance (MonadReader r m, ScottyError e) ⇒ MonadReader r (ActionT e m) where
  ask = lift ask

instance (MonadReader r m, ScottyError e) ⇒ MonadReader r (ScottyT e m) where
  ask = lift ask

initCtx ∷ SweetrollConf → SweetrollTemplates → IO SweetrollCtx
initCtx conf tpls = do
  httpClientMgr ← H.newManager
  sysRandom ← cprgCreate <$> createEntropyPool
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxTpls     = tpls
                      , _ctxHostInfo = [ "domain" .= domainName conf
                                       , "s" .= s conf
                                       , "base_url" .= baseUrl conf ]
                      , _ctxHttpMgr  = httpClientMgr
                      , _ctxRng      = sysRandom }

runSweetrollBase ∷ (MonadSweetroll m, MonadIO m) ⇒ SweetrollBase a → m a
runSweetrollBase x = ask >>= liftIO . runReaderT x

sweetrollApp ∷ SweetrollConf → SweetrollTemplates → SweetrollApp → IO Application
sweetrollApp conf tpls app = do
  ctx ← initCtx conf tpls
  let run x = runReaderT x ctx
  scottyAppT run run app

getConf ∷ MonadSweetroll m ⇒ m SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll m ⇒ (SweetrollConf → a) → m a
getConfOpt f = asks $ f . _ctxConf

getTpls ∷ MonadSweetroll m ⇒ m SweetrollTemplates
getTpls = asks _ctxTpls

getHostInfo ∷ MonadSweetroll m ⇒ m [Pair]
getHostInfo = asks _ctxHostInfo

getRng ∷ MonadSweetroll m ⇒ m SystemRNG
getRng = asks _ctxRng

instance H.HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

-- | Convenient wrapper around Network.HTTP requests.
request ∷ (MonadSweetroll m, MonadIO m, Stringable a) ⇒ H.Request → m (H.Response a)
request req = do
  resp ← H.httpLbs req
  return $ resp { H.responseBody = fromLazyByteString $ H.responseBody resp }

parseUrlP ∷ (MonadIO m) ⇒ String → String → m H.Request
parseUrlP postfix url = liftIO $ H.parseUrl $ url ++ postfix

-- | Returns an action that writes data as application/x-www-form-urlencoded.
showForm ∷ (Stringable a) ⇒ [(a, a)] → SweetrollAction ()
showForm x = do
  status ok200
  setHeader "Content-Type" "application/x-www-form-urlencoded; charset=utf-8"
  raw $ toLazyByteString $ writeForm x

created ∷ LText → SweetrollAction ()
created url = status created201 >> setHeader "Location" url

updated ∷ LText → SweetrollAction ()
updated url = status ok200 >> setHeader "Location" url

unauthorized ∷ SweetrollAction ()
unauthorized = status unauthorized401
