{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, DeriveGeneric #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           Data.Microformats2.Parser

data SweetrollSecrets = SweetrollSecrets
  {                secretKey ∷ Text
  ,          pollSecretToken ∷ Text }

data SweetrollConf = SweetrollConf
  {            mediaEndpoint ∷ String
  ,   indieauthCheckEndpoint ∷ String
  ,              databaseUrl ∷ ByteString
  ,      allowJsCookieAccess ∷ Bool
  ,                 testMode ∷ Bool
  } deriving (Generic, Show)

instance Default SweetrollConf where
  def = SweetrollConf {
        mediaEndpoint            = "/micropub/media"
      , indieauthCheckEndpoint   = "https://indieauth.com/auth"
      , databaseUrl              = "postgres://localhost/sweetroll?sslmode=disable"
      , allowJsCookieAccess      = False
      , testMode                 = False }

instance DefConfig SweetrollConf where
  defConfig = def

instance FromEnv SweetrollConf where
  fromEnv = gFromEnvCustom Option { dropPrefixCount = 0, customPrefix = "" }

instance Default SweetrollSecrets where
  -- the executable sets these to a secure random value by default
  def = SweetrollSecrets {
      pollSecretToken          = ""
    , secretKey                = "SECRET" }

mf2Options ∷ Mf2ParserSettings
mf2Options = def
