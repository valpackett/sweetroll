{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Pages (module Sweetroll.Pages) where

import           ClassyPrelude
import           Text.Pandoc
import           Text.Blaze.Renderer.Text
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Aeson.Types

entryView :: String -> (String, Entry) -> Value
entryView catName (slug, e) = object [
    "name"             .= fromMaybe "" (entryName e)
  , "content"          .= renderContent e
  , "published"        .= fromMaybe "" (formatTimeText <$> entryPublished e)
  , "publishedAttr"    .= fromMaybe "" (formatTimeAttr <$> entryPublished e)
  , "permalink"        .= mconcat ["/", catName, "/", pack slug]
  , "isNote"           .= isNothing (entryName e)
  ]
  where formatTimeText = asLText . pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"
        formatTimeAttr = asLText . pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

catView :: String -> [(String, Entry)] -> Value
catView name entries = object [
    "name" .= name
  , "entries" .= map (entryView name) entries
  ]

renderContent :: Entry -> LText
renderContent e = case fromMaybe (Right "") $ entryContent e of
  Left p -> renderMarkup $ writeHtml def p
  Right t -> t
