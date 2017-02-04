{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad that contains the application context + some
-- functions that use it, such as HTTP client requests.
module Sweetroll.Monads where

import           Sweetroll.Prelude
import           System.Process (readProcessWithExitCode)
import           Data.Maybe
import           Network.HTTP.Client.Conduit
import           Servant
import           Gitson
import qualified Hasql.Pool as HP
import           Sweetroll.Conf

type SweetrollCtx = (SweetrollConf, SweetrollSecrets, TVar [String], MVar (), Manager, HP.Pool)

instance (Has Manager α) ⇒ HasHttpManager α where
  getHttpManager = getter

instance (Has (MVar ()) α, MonadReader α μ) ⇒ HasGitsonLock μ where
  getGitsonLock = asks getter

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
  lck ← newMVar ()
  (_, deleted', _) ← readProcessWithExitCode "git" [ "log", "--all", "--diff-filter=D", "--find-renames", "--name-only", "--pretty=format:" ] ""
  deleted ← newTVarIO $ lines deleted'
  dbp ← HP.acquire (4, 5, "postgres://localhost/sweetroll?sslmode=disable")
  return (conf, secs, deleted, lck, hmg, dbp)


getConf ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ μ SweetrollConf
getConf = asks getter

getConfOpt ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ (SweetrollConf → ψ) → μ ψ
getConfOpt f = asks $ f . getter

getBaseURI ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ μ URI
getBaseURI = asks $ fromMaybe (fromJust $ baseURI def) . baseURI . getter

getSecs ∷ (Has SweetrollSecrets α, MonadReader α μ) ⇒ μ SweetrollSecrets
getSecs = asks getter

getDeleted ∷ (Has (TVar [String]) α, MonadReader α μ) ⇒ μ (TVar [String])
getDeleted = asks getter

runCategoryDeciders ∷ MonadIO μ ⇒ Value → μ Text
runCategoryDeciders v = do
  return "notes" -- XXX

parseEntryURI ∷ (MonadError ServantErr μ, Has SweetrollConf α, MonadReader α μ) ⇒
                URI → μ (String, String)
parseEntryURI uri = do
  base ← getBaseURI
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority uri) == (uriRegName <$> uriAuthority base)
  parseEntryURIRelative uri
