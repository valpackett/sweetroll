{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, IncoherentInstances, TypeOperators, TypeFamilies, FlexibleContexts #-}

-- | The Sweetroll monad contains the application context.
module Sweetroll.Context where

import           Sweetroll.Prelude
import           Sweetroll.Conf
import           Sweetroll.Database (Db, mkDb)

type SweetrollCtx = (SweetrollConf, SweetrollSecrets, ModLogger, ModHttpClient, Db)

type Sweetroll = MagicbaneApp SweetrollCtx

initCtx ∷ SweetrollConf → SweetrollSecrets → IO SweetrollCtx
initCtx conf secs = do
  (_, log) ← newLogger $ LogStderr defaultBufSize
  hmg ← newHttpClient
  dbp ← mkDb conf
  return (conf, secs, log, hmg, dbp)

getConf ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ μ SweetrollConf
getConf = asks getter

getConfOpt ∷ (Has SweetrollConf α, MonadReader α μ) ⇒ (SweetrollConf → ψ) → μ ψ
getConfOpt f = asks $ f . getter

getSecs ∷ (Has SweetrollSecrets α, MonadReader α μ) ⇒ μ SweetrollSecrets
getSecs = asks getter
