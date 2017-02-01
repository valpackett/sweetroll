{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           CMark
import qualified Data.HashMap.Strict as HMS
import           Data.Aeson.TH
import           Data.Microformats2.Parser

newtype SyndicationConfig = MkSyndicationConfig Value

instance ToJSON SyndicationConfig where
  toJSON (MkSyndicationConfig v) = toJSON v

instance FromJSON SyndicationConfig where
  parseJSON v = return $ MkSyndicationConfig v

data SweetrollSecrets = SweetrollSecrets
  {                secretKey ∷ Text
  ,          proxySigningKey ∷ ByteString }

data SweetrollConf = SweetrollConf
  {                 siteName ∷ Maybe Text
  ,               domainName ∷ Maybe Text
  ,               httpsWorks ∷ Maybe Bool
  ,             itemsPerPage ∷ Maybe Int
  ,      categoriesInLanding ∷ Maybe [String]
  ,          categoriesInNav ∷ Maybe [String]
  ,           categoryTitles ∷ Maybe (HashMap Text Text)
  ,        syndicationConfig ∷ Maybe SyndicationConfig
  ,            mediaEndpoint ∷ Maybe String
  ,   indieAuthRedirEndpoint ∷ Maybe String
  ,   indieAuthCheckEndpoint ∷ Maybe String -- Separated for debugging
  ,                  pushHub ∷ Maybe String
  ,                pushDelay ∷ Maybe Int
  ,                 testMode ∷ Maybe Bool }

$(deriveJSON defaultOptions ''SweetrollConf)

instance Default SweetrollConf where
  -- IMPORTANT: No Nothings here!!!
  def = SweetrollConf {
        siteName                 = Just "A new Sweetroll website"
      , httpsWorks               = Just False
      , domainName               = Just "localhost"
      , itemsPerPage             = Just 20
      , categoriesInLanding      = Just [ "articles+notes+stories" ]
      , categoriesInNav          = Just [ "articles+notes+stories", "articles", "replies+likes", "bookmarks" ]
      , categoryTitles           = Just $ HMS.fromList [ ("articles+notes+stories", "Notes and articles")
                                                       , ("articles", "Articles")
                                                       , ("replies+likes", "Responses")
                                                       , ("bookmarks", "Bookmarks") ]
      , syndicationConfig        = Just $ MkSyndicationConfig $ toJSON [
                                       object [ "name" .= asText "twitter.com",   "uid" .= asText "<a href=\"https://brid.gy/publish/twitter\" data-synd></a>" ]
                                     , object [ "name" .= asText "facebook.com",  "uid" .= asText "<a href=\"https://brid.gy/publish/facebook\" data-synd></a>" ]
                                     -- , object [ "name" .= asText "test",          "uid" .= asText "<a href=\"http://localhost:9247/post?type=link&amp;syndication=yep\" data-synd></a>" ]
                                     , object [ "name" .= asText "instagram.com", "uid" .= asText "<a href=\"https://brid.gy/publish/instagram\" data-synd></a>" ]
                                     ]
      , mediaEndpoint            = Just "/micropub/media"
      , indieAuthCheckEndpoint   = Just "https://indieauth.com/auth"
      , indieAuthRedirEndpoint   = Just "https://indieauth.com/auth"
      , pushHub                  = Just "https://switchboard.p3k.io"
      , pushDelay                = Just 3
      , testMode                 = Just False }

instance Default SweetrollSecrets where
  def = SweetrollSecrets {
        secretKey                = "SECRET" -- the executable sets to a secure random value by default
      , proxySigningKey          = "SECRET" }

baseURI ∷ SweetrollConf → Maybe URI
baseURI conf = asum [ parseURI $ (if fromMaybe False (httpsWorks conf) then "https://" else "http://") ++ unpack (fromMaybe "" $ domainName conf) -- need to parse because we need the :PORT parsed correctly for dev
                    , parseURI $ unpack $ fromMaybe "" $ domainName conf -- in case someone puts "https://" in the field which is clearly called DOMAIN NAME
                    , Just $ URI (if fromMaybe False (httpsWorks conf) then "https:" else "http:") (Just $ URIAuth "" (cs $ fromMaybe "" $ domainName conf) "") "" "" "" ]

mf2Options ∷ Mf2ParserSettings
mf2Options = def

cmarkOptions ∷ [CMarkOption]
cmarkOptions = [ optNormalize, optSmart ]
