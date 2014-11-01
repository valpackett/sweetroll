{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Pages (
  CategoryName
, EntrySlug
, ViewResult
, tplContext
, titleParts
, entryView
, catView
, indexView
) where

import           ClassyPrelude
import           Text.Pandoc
import           Text.Blaze.Renderer.Text
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Aeson.Types

type CategoryName = String
type EntrySlug = String

data ViewResult = ViewResult
  { titleParts           :: [Text]
  , tplContext           :: Value }

entryView :: CategoryName -> (EntrySlug, Entry) -> ViewResult
entryView catName (slug, e) =
  ViewResult { titleParts = [toStrict (fromMaybe ("Note @ " ++ published) (entryName e)), pack catName]
             , tplContext = ctx }
  where content = renderContent e
        published = fromMaybe "" (formatTimeText <$> entryPublished e)
        ctx = object [
            "name"             .= fromMaybe "" (entryName e)
          , "content"          .= content
          , "published"        .= published
          , "publishedAttr"    .= fromMaybe "" (formatTimeAttr <$> entryPublished e)
          , "permalink"        .= mconcat ["/", catName, "/", pack slug]
          , "isNote"           .= isNothing (entryName e)
          ]

catView :: CategoryName -> [(EntrySlug, Entry)] -> ViewResult
catView name entries =
  ViewResult { titleParts = [pack name]
             , tplContext = ctx }
  where ctx = object [
            "name"            .= name
          , "permalink"       .= mconcat ["/", name]
          , "entries"         .= map (tplContext . entryView name) entries
          ]

indexView :: [(CategoryName, [(EntrySlug, Entry)])] -> ViewResult
indexView cats =
  ViewResult { titleParts = []
             , tplContext = ctx }
  where ctx = object [
            "categories" .= map (tplContext . uncurry catView) cats
          ]

renderContent :: Entry -> LText
renderContent e = case fromMaybe (Right "") $ entryContent e of
  Left p -> renderMarkup $ writeHtml def p
  Right t -> t

formatTimeText :: UTCTime -> LText
formatTimeText = asLText . pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

formatTimeAttr :: UTCTime -> LText
formatTimeAttr = asLText . pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
