{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

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
, notFoundView
, renderContent
, renderRaw
, render
) where

import           ClassyPrelude
import           Text.Pandoc hiding (Template, renderTemplate)
import qualified Web.Scotty.Trans as SC
import           Web.Simple.Templates.Language
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Aeson.Types
import qualified Data.Text.Lazy as LT
import           Data.List (elemIndex)
import           Safe (atMay)
import           Sweetroll.Pagination
import           Sweetroll.Conf
import           Sweetroll.Util
import           Sweetroll.Monads

data ViewResult = ViewResult
  { titleParts           ∷ [Text]
  , tplContext           ∷ Value }

entryView ∷ CategoryName → [EntrySlug] → (EntrySlug, Entry) → ViewResult
entryView catName otherSlugs (slug, e) =
  ViewResult { titleParts = [toStrict (fromMaybe ("Note @ " ++ published) (entryName e)), pack catName]
             , tplContext = ctx }
  where content = renderContent writeHtmlString e
        published = fromMaybe "" (formatTimeText <$> entryPublished e)
        slugIdx = fromMaybe (negate 1) $ elemIndex slug otherSlugs
        prev = atMay otherSlugs $ slugIdx - 1
        next = atMay otherSlugs $ slugIdx + 1
        twitterId = lastMay =<< LT.splitOn "/" <$> find (isInfixOf "twitter.com") (entrySyndication e)
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
          , "hasTwitterId"     .= isJust twitterId
          , "twitterId"        .= fromMaybe "" twitterId
          , "isReply"          .= isJust (entryInReplyTo e)
          , "replyForUrl"      .= fromMaybe "" (derefEntry =<< entryInReplyTo e)
          , "replyForName"     .= fromMaybe "" (derefEntryName =<< entryInReplyTo e)
          -- TODO: repost/like
          ]

catView ∷ SweetrollConf → CategoryName → Page (EntrySlug, Entry) → ViewResult
catView conf name page =
  ViewResult { titleParts = [pack name]
             , tplContext = ctx }
  where entries = items page
        slugs = map fst entries
        ctx = object [
            "name"            .= name
          , "permalink"       .= mconcat ["/", name]
          , "entries"         .= map (tplContext . entryView name slugs) entries
          , "renderedEntries" .= map (renderTemplate (entryInListTemplate conf) helpers . tplContext . entryView name slugs) entries
          , "firstHref"       .= (pageLink  $ firstPage page)
          , "shouldFirst"     .= (thisPage page > firstPage page)
          , "hasPrev"         .= (isJust    $ prevPage page)
          , "prevHref"        .= (pageLink' $ prevPage page)
          , "allPages"        .= map pageLink [firstPage page .. lastPage page]
          , "hasNext"         .= (isJust    $ nextPage page)
          , "nextHref"        .= (pageLink' $ nextPage page)
          , "lastHref"        .= (pageLink  $ lastPage page)
          , "shouldLast"      .= (thisPage page < lastPage page)
          ]
        pageLink n = mconcat ["/", name, "?page=", show n]
        pageLink' = pageLink . fromMaybe 0

indexView ∷ SweetrollConf → [(CategoryName, Page (EntrySlug, Entry))] → ViewResult
indexView conf cats =
  ViewResult { titleParts = []
             , tplContext = ctx }
  where ctx = object [
            "categories" .= map (tplContext . uncurry (catView conf)) cats
          ]

notFoundView ∷ ViewResult
notFoundView = ViewResult { titleParts = ["404"], tplContext = object [] }

renderContent ∷ (WriterOptions → Pandoc → String) → Entry → LText
renderContent writer e = case fromMaybe (Right "") $ entryContent e of
  Left p → pack $ writer pandocWriterOptions p
  Right t → t

renderRaw ∷ Template → [Pair] → Text
renderRaw t c = renderTemplate t helpers $ object c

render ∷ SweetrollConf → Text → (SweetrollConf → Template) → ViewResult → SweetrollAction ()
render conf authorHtml tplf stuff = do
  hostInfo ← getHostInfo
  let ctx = object $ hostInfo ++ [
                "content" .= renderTemplate (tplf conf) helpers (tplContext stuff)
              , "author" .= authorHtml
              , "website_title" .= siteName conf
              , "meta_title" .= intercalate (titleSeparator conf) (titleParts stuff ++ [siteName conf])
              ]
  SC.html $ fromStrict $ renderTemplate (layoutTemplate conf) helpers ctx

helpers ∷ FunctionMap
helpers = mapFromList [ ("syndicationName", toFunction syndicationName) ]

syndicationName ∷ Text → Value
syndicationName u | isInfixOf "app.net"     u = toJSON $ asText "App.net"
                  | isInfixOf "twitter.com" u = toJSON $ asText "Twitter"
                  | otherwise                 = toJSON $ u

formatTimeText ∷ UTCTime → LText
formatTimeText = asLText . pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

formatTimeAttr ∷ UTCTime → LText
formatTimeAttr = asLText . pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
