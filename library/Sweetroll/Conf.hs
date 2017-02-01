{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveGeneric #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           System.Envy hiding ((.=))
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
  {               domainName ∷ Text
  ,               httpsWorks ∷ Bool
  ,            mediaEndpoint ∷ String
  ,   indieAuthRedirEndpoint ∷ String
  ,   indieAuthCheckEndpoint ∷ String -- Separated for debugging
  ,                  pushHub ∷ String
  ,                pushDelay ∷ Int
  ,                 testMode ∷ Bool
  } deriving (Generic, Show)

instance Default SweetrollConf where
  def = SweetrollConf {
        httpsWorks               = False
      , domainName               = "localhost"
      , mediaEndpoint            = "/micropub/media"
      , indieAuthCheckEndpoint   = "https://indieauth.com/auth"
      , indieAuthRedirEndpoint   = "https://indieauth.com/auth"
      , pushHub                  = "https://switchboard.p3k.io"
      , pushDelay                = 3
      , testMode                 = False }

instance DefConfig SweetrollConf where
  defConfig = def

instance FromEnv SweetrollConf where
  fromEnv = gFromEnvCustom Option { dropPrefixCount = 0, customPrefix = "SWEETROLL" }

instance Default SweetrollSecrets where
  def = SweetrollSecrets {
        secretKey                = "SECRET" } -- the executable sets to a secure random value by default

baseURI ∷ SweetrollConf → Maybe URI
baseURI conf = asum [ parseURI $ (if httpsWorks conf then "https://" else "http://") ++ unpack (domainName conf) -- need to parse because we need the :PORT parsed correctly for dev
                    , parseURI $ unpack $ domainName conf -- in case someone puts "https://" in the field which is clearly called DOMAIN NAME
                    , Just $ URI (if httpsWorks conf then "https:" else "http:") (Just $ URIAuth "" (cs $ domainName conf) "") "" "" "" ]

mf2Options ∷ Mf2ParserSettings
mf2Options = def

cmarkOptions ∷ [CMarkOption]
cmarkOptions = [ optNormalize, optSmart ]
