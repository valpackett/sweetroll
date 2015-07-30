{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Rendering where

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
import           Sweetroll.Pages
import           Sweetroll.Routes
import           Sweetroll.Conf
import           Sweetroll.Util
import           Sweetroll.Monads

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML IndieConfig where
  mimeRender _ ic =
    "<!DOCTYPE html><meta charset=utf-8><script>(parent!==window)?parent.postMessage(JSON.stringify("
    ++ encode ic ++ "),'*'):navigator.registerProtocolHandler('web+action',"
    ++ "location.protocol+'//'+location.hostname+location.pathname+'?handler=%s','Sweetroll')</script>"

instance MimeRender HTML Text where
  mimeRender _ = toLazyByteString

instance Templatable α ⇒ MimeRender HTML (View α) where
  mimeRender x = mimeRender x . renderInLayout

instance Accept CSS where
  contentType _ = "text" // "css"

instance Stringable α ⇒ MimeRender CSS α where
  mimeRender _ = toLazyByteString


view ∷ α → Sweetroll (View α)
view content = do
  conf ← getConf
  tpls ← getTpls
  hostInfo ← getHostInfo
  return $ View conf tpls hostInfo content

data ViewResult = ViewResult
  { resultHtml    ∷ Text
  , resultTitle   ∷ [Text]
  , resultContext ∷ Value }

class Templatable α where
  renderBare ∷ (SweetrollTemplates → Template) → View α → ViewResult
  templateInLayout ∷ View α → (SweetrollTemplates → Template)
  renderInLayout ∷ View α → Text
  renderInLayout v@(View conf tpls hostInfo _) = renderTemplate (layoutTemplate tpls) helpers ctx
      where ctx = object $ hostInfo ++ [
                      "content"        .= innerHtml
                    , "author"         .= renderRaw (authorTemplate tpls) hostInfo
                    , "website_title"  .= siteName conf
                    , "meta_title"     .= intercalate (titleSeparator conf) (titleParts ++ [siteName conf])
                    ]
            (ViewResult innerHtml titleParts _) = renderBare (templateInLayout v) v

instance Templatable EntryPage where
  templateInLayout _ = entryTemplate
  renderBare tplf (View _ tpls _ (EntryPage catName otherSlugs (slug, e))) = ViewResult html titleParts ctx
    where html = renderTemplate (tplf tpls) helpers ctx
          titleParts = [toStrict (fromMaybe ("Note @ " ++ published) . headMay $ entryName e), pack catName]
          ctx = object [
              "name"             .= orEmpty (entryName e)
            , "content"          .= content
            , "published"        .= published
            , "publishedAttr"    .= orEmpty (formatTimeAttr <$> entryPublished e)
            , "permalink"        .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack slug)
            , "isUntitled"       .= (null . entryName $ e)
            , "category"         .= catName
            , "categoryHref"     .= showLink (permalink (Proxy ∷ Proxy CatRoute) catName (-1))
            , "hasPrev"          .= isJust prev
            , "prevHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe prev)
            , "hasNext"          .= isJust next
            , "nextHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe next)
            , "hasSyndication"   .= (not . null . entrySyndication $ e)
            , "syndication"      .= entrySyndication e
            , "hasTwitterId"     .= isJust twitterId
            , "twitterId"        .= orEmptyMaybe twitterId
            , "isReply"          .= (not . null . entryInReplyTo $ e)
            , "replyForUrl"      .= orEmptyMaybe (derefEntry =<< headMay (entryInReplyTo e))
            , "replyForName"     .= orEmptyMaybe (derefEntryName =<< headMay (entryInReplyTo e))
            -- TODO: repost/like
            ]
          content = renderContent writeHtmlString e
          published = orEmpty $ formatTimeText <$> entryPublished e
          slugIdx = fromMaybe (-1) $ elemIndex slug otherSlugs
          prev = atMay otherSlugs $ slugIdx - 1
          next = atMay otherSlugs $ slugIdx + 1
          twitterId = lastMay =<< LT.splitOn "/" <$> find (isInfixOf "twitter.com") (entrySyndication e)

instance Templatable CatPage where
  templateInLayout _ = categoryTemplate
  renderBare tplf (View conf tpls hostInfo (CatPage name page)) = ViewResult html titleParts ctx
    where html = renderTemplate (tplf tpls) helpers ctx
          titleParts = [pack name]
          ctx = object [
              "name"            .= name
            , "permalink"       .= pageLink (-1)
            , "entries"         .= map resultContext entryResults
            , "renderedEntries" .= map resultHtml entryResults
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
          entryResults = map entryHtml entries
          entryHtml = renderBare entryInListTemplate . View conf tpls hostInfo . EntryPage name slugs
          slugs = map fst entries
          entries = items page
          pageLink n = showLink (permalink (Proxy ∷ Proxy CatRoute) name n)
          pageLink' = pageLink . fromMaybe 0

instance Templatable IndexPage where
  templateInLayout _ = indexTemplate
  renderBare tplf (View conf tpls hostInfo (IndexPage cats)) = ViewResult html titleParts ctx
    where html = renderTemplate (tplf tpls) helpers ctx
          titleParts = []
          ctx = object [ "categories" .= map catContext cats ]
          catContext (n, p) = resultContext . renderBare categoryTemplate . View conf tpls hostInfo $ CatPage n p

permalink ∷ (IsElem endpoint SweetrollAPI, HasLink endpoint) ⇒ Proxy endpoint → MkLink endpoint
permalink = safeLink sweetrollAPI

showLink ∷ Show α ⇒ α → Text
showLink = ("/" ++) . Data.Stringable.toText . show

renderContent ∷ (WriterOptions → Pandoc → String) → Entry → LText
renderContent writer e = case headMay $ entryContent e of
  Just (PandocContent p) → pack $ writer pandocWriterOptions p
  Just (TextContent t) → t
  _ -> ""

renderRaw ∷ Template → [Pair] → Text
renderRaw t = renderTemplate t helpers . object

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
