{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, IncoherentInstances, TypeOperators, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad contains the application context.
module Sweetroll.Context where

import           Sweetroll.Prelude
import           Network.HTTP.Client.Conduit
import           Servant
import           Sweetroll.Conf
import           Sweetroll.Database (Db, mkDb)

type SweetrollCtx = (SweetrollConf, SweetrollSecrets, Manager, Db)

instance (Has Manager α) ⇒ HasHttpManager α where
  getHttpManager = getter

newtype Sweetroll α = Sweetroll {
  runSweetroll ∷ ReaderT SweetrollCtx (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadCatch, MonadError ServantErr, MonadReader SweetrollCtx)

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
  dbp ← mkDb conf
  return (conf, secs, hmg, dbp)


getConf ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ μ SweetrollConf
getConf = asks getter

getConfOpt ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ (SweetrollConf → ψ) → μ ψ
getConfOpt f = asks $ f . getter

getSecs ∷ (Has SweetrollSecrets α, MonadReader α μ) ⇒ μ SweetrollSecrets
getSecs = asks getter

getDeleted ∷ (Has (TVar [String]) α, MonadReader α μ) ⇒ μ (TVar [String])
getDeleted = asks getter
