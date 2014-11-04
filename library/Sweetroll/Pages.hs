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
, renderContent
, renderRaw
, renderWithConf
) where

import           ClassyPrelude
import           Text.Pandoc hiding (Template, renderTemplate)
import qualified Web.Scotty as SC
import           Web.Simple.Templates.Language
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Aeson.Types
import           Data.List (elemIndex)
import           Safe (atMay)
import           Sweetroll.Conf
import           Sweetroll.Util

type CategoryName = String
type EntrySlug = String

data ViewResult = ViewResult
  { titleParts           :: [Text]
  , tplContext           :: Value }

entryView :: CategoryName -> [EntrySlug] -> (EntrySlug, Entry) -> ViewResult
entryView catName otherSlugs (slug, e) =
  ViewResult { titleParts = [toStrict (fromMaybe ("Note @ " ++ published) (entryName e)), pack catName]
             , tplContext = ctx }
  where content = renderContent writeHtmlString e
        published = fromMaybe "" (formatTimeText <$> entryPublished e)
        slugIdx = fromMaybe (negate 1) $ elemIndex slug otherSlugs
        prev = atMay otherSlugs $ slugIdx - 1
        next = atMay otherSlugs $ slugIdx + 1
        ctx = object [
            "name"             .= fromMaybe "" (entryName e)
          , "content"          .= content
          , "published"        .= published
          , "publishedAttr"    .= fromMaybe "" (formatTimeAttr <$> entryPublished e)
          , "permalink"        .= mconcat ["/", catName, "/", pack slug]
          , "isNote"           .= isNothing (entryName e)
          , "category"         .= catName
          , "categoryHref"     .= mconcat ["/", catName]
          , "hasPrev"          .= isJust prev
          , "prevHref"         .= mconcat ["/", catName, "/", fromMaybe "" prev]
          , "hasNext"          .= isJust next
          , "nextHref"         .= mconcat ["/", catName, "/", fromMaybe "" next]
          , "hasSyndication"   .= (not $ null $ entrySyndication e)
          , "syndication"      .= entrySyndication e
          ]

catView :: CategoryName -> [(EntrySlug, Entry)] -> ViewResult
catView name entries =
  ViewResult { titleParts = [pack name]
             , tplContext = ctx }
  where slugs = map fst entries
        ctx = object [
            "name"            .= name
          , "permalink"       .= mconcat ["/", name]
          , "entries"         .= map (tplContext . entryView name slugs) entries
          ]

indexView :: [(CategoryName, [(EntrySlug, Entry)])] -> ViewResult
indexView cats =
  ViewResult { titleParts = []
             , tplContext = ctx }
  where ctx = object [
            "categories" .= map (tplContext . uncurry catView) cats
          ]

renderContent :: (WriterOptions -> Pandoc -> String) -> Entry -> LText
renderContent writer e = case fromMaybe (Right "") $ entryContent e of
  Left p -> pack $ writer def p
  Right t -> t

renderRaw :: Template -> [Pair] -> Text
renderRaw t c = renderTemplate t helpers $ object c

renderWithConf :: SweetrollConf -> Text -> [Pair] -> (SweetrollConf -> Template) -> ViewResult -> SweetrollAction ()
renderWithConf conf authorHtml hostInfo tplf stuff = SC.html $ fromStrict $ renderTemplate (layoutTemplate conf) helpers ctx
  where ctx = object $ hostInfo ++ [
                "content" .= renderTemplate (tplf conf) helpers (tplContext stuff)
              , "author" .= authorHtml
              , "website_title" .= siteName conf
              , "meta_title" .= intercalate (titleSeparator conf) (titleParts stuff ++ [siteName conf])
              ]

helpers :: FunctionMap
helpers = mapFromList [ ("syndicationName", toFunction syndicationName) ]

syndicationName :: Text -> Value
syndicationName u | isInfixOf "app.net"     u = toJSON $ asText "App.net"
                  | isInfixOf "twitter.com" u = toJSON $ asText "Twitter"
                  | otherwise                 = toJSON $ u

formatTimeText :: UTCTime -> LText
formatTimeText = asLText . pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

formatTimeAttr :: UTCTime -> LText
formatTimeAttr = asLText . pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
