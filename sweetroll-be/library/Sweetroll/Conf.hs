{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, DeriveGeneric #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           CMark
import           Data.Microformats2.Parser

data SweetrollSecrets = SweetrollSecrets
  {                secretKey ∷ Text }

data SweetrollConf = SweetrollConf
  {            mediaEndpoint ∷ String
  ,   indieAuthCheckEndpoint ∷ String
  ,                 testMode ∷ Bool
  } deriving (Generic, Show)

instance Default SweetrollConf where
  def = SweetrollConf {
        mediaEndpoint            = "/micropub/media"
      , indieAuthCheckEndpoint   = "https://indieauth.com/auth"
      , testMode                 = False }

instance FromEnv SweetrollConf where
  fromEnv = gFromEnvCustom Option { dropPrefixCount = 0, customPrefix = "SWEETROLL" }

instance Default SweetrollSecrets where
  def = SweetrollSecrets {
        secretKey                = "SECRET" } -- the executable sets to a secure random value by default

mf2Options ∷ Mf2ParserSettings
mf2Options = def

cmarkOptions ∷ [CMarkOption]
cmarkOptions = [ optNormalize, optSmart ]
