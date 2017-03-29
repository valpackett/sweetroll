{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, DeriveGeneric #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           CMark
import           Data.Microformats2.Parser

newtype SyndicationConfig = MkSyndicationConfig Value

instance ToJSON SyndicationConfig where
  toJSON (MkSyndicationConfig v) = toJSON v

instance FromJSON SyndicationConfig where
  parseJSON v = return $ MkSyndicationConfig v

syndicationConfig ∷ SyndicationConfig
syndicationConfig = MkSyndicationConfig $ toJSON [
    object [ "name" .= asText "twitter.com",   "uid" .= asText "<a href=\"https://brid.gy/publish/twitter\" data-synd></a>" ]
  , object [ "name" .= asText "facebook.com",  "uid" .= asText "<a href=\"https://brid.gy/publish/facebook\" data-synd></a>" ]
  -- , object [ "name" .= asText "test",          "uid" .= asText "<a href=\"http://localhost:9247/post?type=link&amp;syndication=yep\" data-synd></a>" ]
  , object [ "name" .= asText "instagram.com", "uid" .= asText "<a href=\"https://brid.gy/publish/instagram\" data-synd></a>" ]
  ]


data SweetrollSecrets = SweetrollSecrets
  {                secretKey ∷ Text }

data SweetrollConf = SweetrollConf
  {               httpsWorks ∷ Bool
  ,            mediaEndpoint ∷ String
  ,   indieAuthRedirEndpoint ∷ String
  ,   indieAuthCheckEndpoint ∷ String -- Separated for debugging
  ,                 testMode ∷ Bool
  } deriving (Generic, Show)

instance Default SweetrollConf where
  def = SweetrollConf {
        httpsWorks               = False
      , mediaEndpoint            = "/micropub/media"
      , indieAuthCheckEndpoint   = "https://indieauth.com/auth"
      , indieAuthRedirEndpoint   = "https://indieauth.com/auth"
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
