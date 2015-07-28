{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Pages (
  HTML
, CSS
, CategoryName
, EntrySlug
, ViewResult
, tplContext
, titleParts
, entryView
, renderEntry
, catView
, renderCat
, indexView
, renderIndex
, notFoundView
, renderContent
, renderRaw
, render
) where

import           ClassyPrelude
import           Text.Pandoc hiding (Template, renderTemplate)
import           Web.Simple.Templates.Language
import           Network.HTTP.Media.MediaType
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Aeson (encode)
import           Data.Aeson.Types
import qualified Data.Text.Lazy as LT
import           Data.List (elemIndex)
import           Data.Stringable
import           Safe (atMay)
import           Servant
import           Sweetroll.Pagination
import           Sweetroll.Conf
import           Sweetroll.Util
import           Sweetroll.Monads

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML IndieConfig where
  mimeRender _ ic =
    "<!DOCTYPE html><meta charset=utf-8><script>(parent!==window)?parent.postMessage(JSON.stringify("
    ++ encode ic ++ "),'*'):navigator.registerProtocolHandler('web+action',"
    ++ "location.protocol+'//'+location.hostname+location.pathname+'?handler=%s','Sweetroll')</script>"

instance MimeRender HTML Text where
  mimeRender _ = toLazyByteString

data CSS

instance Accept CSS where
  contentType _ = "text" // "css"

instance Stringable α ⇒ MimeRender CSS α where
  mimeRender _ = toLazyByteString


data ViewResult = ViewResult
  { titleParts           ∷ [Text]
  , tplContext           ∷ Value }

entryView ∷ CategoryName → [EntrySlug] → (EntrySlug, Entry) → ViewResult
entryView catName otherSlugs (slug, e) =
  ViewResult { titleParts = [toStrict (fromMaybe ("Note @ " ++ published) . headMay $ entryName e), pack catName]
             , tplContext = ctx }
  where content = renderContent writeHtmlString e
        published = orEmpty $ formatTimeText <$> entryPublished e
        slugIdx = fromMaybe (negate 1) $ elemIndex slug otherSlugs
        prev = atMay otherSlugs $ slugIdx - 1
        next = atMay otherSlugs $ slugIdx + 1
        twitterId = lastMay =<< LT.splitOn "/" <$> find (isInfixOf "twitter.com") (entrySyndication e)
        ctx = object [
            "name"             .= orEmpty (entryName e)
          , "content"          .= content
          , "published"        .= published
          , "publishedAttr"    .= orEmpty (formatTimeAttr <$> entryPublished e)
          , "permalink"        .= mconcat ["/", catName, "/", pack slug]
          , "isNote"           .= (null . entryName $ e)
          , "category"         .= catName
          , "categoryHref"     .= mconcat ["/", catName]
          , "hasPrev"          .= isJust prev
          , "prevHref"         .= mconcat ["/", catName, "/", orEmptyMaybe prev]
          , "hasNext"          .= isJust next
          , "nextHref"         .= mconcat ["/", catName, "/", orEmptyMaybe next]
          , "hasSyndication"   .= (not . null . entrySyndication $ e)
          , "syndication"      .= entrySyndication e
          , "hasTwitterId"     .= isJust twitterId
          , "twitterId"        .= orEmptyMaybe twitterId
          , "isReply"          .= (not . null . entryInReplyTo $ e)
          , "replyForUrl"      .= orEmptyMaybe (derefEntry =<< headMay (entryInReplyTo e))
          , "replyForName"     .= orEmptyMaybe (derefEntryName =<< headMay (entryInReplyTo e))
          -- TODO: repost/like
          ]

renderEntry ∷ CategoryName → [EntrySlug] → (EntrySlug, Entry) → Sweetroll Text
renderEntry catName otherSlugs entry =
  render entryTemplate $ entryView catName otherSlugs entry

catView ∷ SweetrollTemplates → CategoryName → Page (EntrySlug, Entry) → ViewResult
catView tpls name page =
  ViewResult { titleParts = [pack name]
             , tplContext = ctx }
  where entries = items page
        slugs = map fst entries
        ctx = object [
            "name"            .= name
          , "permalink"       .= mconcat ["/", name]
          , "entries"         .= map (tplContext . entryView name slugs) entries
          , "renderedEntries" .= map (renderTemplate (entryInListTemplate tpls) helpers . tplContext . entryView name slugs) entries
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

renderCat ∷ CategoryName → Page (EntrySlug, Entry) → Sweetroll Text
renderCat name page = do
  tpls ← getTpls
  render categoryTemplate $ catView tpls name page

indexView ∷ SweetrollTemplates → [(CategoryName, Page (EntrySlug, Entry))] → ViewResult
indexView tpls cats =
  ViewResult { titleParts = []
             , tplContext = ctx }
  where ctx = object [
            "categories" .= map (tplContext . uncurry (catView tpls)) cats
          ]

renderIndex ∷ [(CategoryName, Page (EntrySlug, Entry))] → Sweetroll Text
renderIndex cats = do
  tpls ← getTpls
  render indexTemplate $ indexView tpls cats

notFoundView ∷ ViewResult
notFoundView = ViewResult { titleParts = ["404"], tplContext = object [] }

renderContent ∷ (WriterOptions → Pandoc → String) → Entry → LText
renderContent writer e = case headMay $ entryContent e of
  Just (PandocContent p) → pack $ writer pandocWriterOptions p
  Just (TextContent t) → t
  _ -> ""

renderRaw ∷ Template → [Pair] → Text
renderRaw t c = renderTemplate t helpers $ object c

render ∷ (SweetrollTemplates → Template) → ViewResult → Sweetroll Text
render tplf vr = do
  conf ← getConf
  tpls ← getTpls
  hostInfo ← getHostInfo
  let ctx = object $ hostInfo ++ [
                "content"        .= renderTemplate (tplf tpls) helpers (tplContext vr)
              , "author"         .= renderRaw (authorTemplate tpls) hostInfo
              , "website_title"  .= siteName conf
              , "meta_title"     .= intercalate (titleSeparator conf) (titleParts vr ++ [siteName conf])
              ]
  return $ renderTemplate (layoutTemplate tpls) helpers ctx

helpers ∷ FunctionMap
helpers = mapFromList [ ("syndicationName", toFunction syndicationName) ]

syndicationName ∷ Text → Value
syndicationName u | "app.net"     `isInfixOf` u = toJSON $ asText "App.net"
                  | "twitter.com" `isInfixOf` u = toJSON $ asText "Twitter"
                  | otherwise                   = toJSON u

formatTimeText ∷ UTCTime → LText
formatTimeText = asLText . pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

formatTimeAttr ∷ UTCTime → LText
formatTimeAttr = asLText . pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
