{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- because Data.Setters
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           Sweetroll.Prelude
import           Text.Pandoc.Options
import           Text.Highlighting.Kate.Styles (tango)
import           Data.Setters
import qualified Data.HashMap.Strict as HMS
import           Data.Aeson.TH
import           Data.Microformats2.Parser
import           Data.FileEmbed

newtype IndieConfig = MkIndieConfig Value

instance ToJSON IndieConfig where
  toJSON (MkIndieConfig v) = toJSON v

instance FromJSON IndieConfig where
  parseJSON v = return $ MkIndieConfig v

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
  ,              indieConfig ∷ Maybe IndieConfig
  ,        syndicationConfig ∷ Maybe SyndicationConfig
  ,   indieAuthRedirEndpoint ∷ Maybe String
  ,   indieAuthCheckEndpoint ∷ Maybe String -- Separated for debugging
  ,                  pushHub ∷ Maybe String
  ,                pushDelay ∷ Maybe Int
  ,                 testMode ∷ Maybe Bool }

$(declareSetters ''SweetrollConf)
$(deriveJSON defaultOptions ''SweetrollConf)

instance Default SweetrollConf where
  -- IMPORTANT: No Nothings here!!!
  def = SweetrollConf {
        siteName                 = Just "A new Sweetroll website"
      , httpsWorks               = Just False
      , domainName               = Just "localhost"
      , itemsPerPage             = Just 20
      , categoriesInLanding      = Just [ "articles+notes" ]
      , categoriesInNav          = Just [ "articles+notes", "articles", "replies+likes" ]
      , categoryTitles           = Just $ HMS.fromList [ ("articles+notes", "Notes and articles")
                                                       , ("articles", "Articles")
                                                       , ("replies+likes", "Responses") ]
      , indieConfig              = Just $ MkIndieConfig $ object [
                                       "reply"    .= asText "https://quill.p3k.io/new?reply={url}"
                                     , "bookmark" .= asText "https://quill.p3k.io/bookmark?url={url}"
                                     , "like"     .= asText "https://quill.p3k.io/favorite?url={url}"
                                     , "repost"   .= asText "https://quill.p3k.io/repost?url={url}" ]
      , syndicationConfig        = Just $ MkSyndicationConfig $ object [
                                       "twitter.com"   .= asText "<a href=\"https://brid.gy/publish/twitter\"></a>"
                                     , "facebook.com"  .= asText "<a href=\"https://brid.gy/publish/facebook\"></a>"
                                     -- , "test"          .= asText "<a href=\"http://localhost:9247/post?type=link&amp;syndication=yep\"></a>"
                                     , "instagram.com" .= asText "<a href=\"https://brid.gy/publish/instagram\"></a>" ]
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

pandocWriterOptions ∷ WriterOptions
pandocWriterOptions = def { writerHtml5 = True
                          , writerEmailObfuscation = NoObfuscation
                          , writerHighlight = True
                          , writerHighlightStyle = tango
                          , writerIdentifierPrefix = "sr-" }

mf2Options ∷ Mf2ParserSettings
mf2Options = def

bowerComponents, defaultTemplates ∷ [(FilePath, ByteString)]
bowerComponents = $(embedDir "bower_components")
defaultTemplates = $(embedDir "templates")
