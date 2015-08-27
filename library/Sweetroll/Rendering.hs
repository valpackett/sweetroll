{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Rendering where

import           ClassyPrelude hiding (fromString)
import           Control.Lens hiding (Index, re, parts, (.=))
import           Web.Simple.Templates.Language
import           Network.HTTP.Media.MediaType
import           Data.Aeson (encode)
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.List (elemIndex)
import qualified Data.Vector as V
import qualified Data.Text as T
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
          titleParts = [ fromMaybe ("Note @ " ++ fromMaybe "" (formatTimeText <$> published)) entryName, pack catName ]
          ctx = object [
              "name"             .= orEmptyMaybe entryName
            , "content"          .= content
            , "hasContent"       .= not (null content)
            , "published"        .= asLText (fromMaybe "" (formatTimeText <$> published))
            , "publishedAttr"    .= asLText (fromMaybe "" (formatTimeAttr <$> published))
            , "permalink"        .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack slug)
            , "isUntitled"       .= null entryName
            , "category"         .= catName
            , "categoryHref"     .= showLink (dropQueryFragment $ permalink (Proxy ∷ Proxy CatRoute) catName (-1))
            , "hasPrev"          .= isJust prev
            , "prevHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe prev)
            , "hasNext"          .= isJust next
            , "nextHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe next)
            , "syndication"      .= entrySyndication
            , "hasSyndication"   .= not (null entrySyndication)
            , "hasTwitterId"     .= isJust twitterId
            , "twitterId"        .= orEmptyMaybe twitterId
            , "replyContexts"    .= map referenceContext entryInReplyTo
            , "likeContexts"     .= map referenceContext entryLikeOf
            , "isLike"           .= not (null entryLikeOf)
            , "repostContexts"   .= map referenceContext entryRepostOf
            ]
          entryName = e ^? key "properties" . key "name" . nth 0 . _String
          content = orEmptyMaybe $ e ^? key "properties" . key "content" . nth 0 . key "html" . _String
          published = parseISOTime =<< e ^? key "properties" . key "published" . nth 0 . _String
          slugIdx = fromMaybe (-1) $ elemIndex slug otherSlugs
          prev = atMay otherSlugs $ slugIdx - 1
          next = atMay otherSlugs $ slugIdx + 1
          twitterId = lastMay =<< T.splitOn "/" <$> find ("twitter.com" `isInfixOf`) entrySyndication
          entrySyndication = mapMaybe (^? _String) $ V.toList $ fromMaybe V.empty $ e ^? key "properties" . key "syndication" . _Array
          entryInReplyTo   = V.toList $ fromMaybe V.empty $ e ^? key "properties" . key "in-reply-to" . _Array
          entryLikeOf      = V.toList $ fromMaybe V.empty $ e ^? key "properties" . key "like-of" . _Array
          entryRepostOf    = V.toList $ fromMaybe V.empty $ e ^? key "properties" . key "repost-of" . _Array

-- XXX: authors aren't displayed; also this is ridiculous
referenceContext ∷ Value → Value
referenceContext v@(Object _) = object [ "url" .= fromMaybe "" (v ^? key "properties" . key "url" . nth 0 . _String)
                                       , "name" .= fromMaybe "" (v ^? key "properties" . key "name" . nth 0 . _String)
                                       , "content" .= fromMaybe "" (v ^? key "properties" . key "content" . nth 0 . key "html" . _String)]
referenceContext (String x) = object [ "url" .= x, "name" .= x ]
referenceContext _ = object [ ]

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
          ctx = object [ "categories" .= map catContext (sortOn (fromMaybe 999 . flip elemIndex (categoryOrder conf) . fst) cats) ]
          catContext (n, p) = resultContext . renderBare categoryTemplate . View conf tpls hostInfo $ CatPage n p

renderRaw ∷ Template → [Pair] → Text
renderRaw t = renderTemplate t helpers . object

helpers ∷ FunctionMap
helpers = mapFromList [ ("syndicationName", toFunction syndicationName) ]

syndicationName ∷ Text → Value
syndicationName u | "app.net"       `isInfixOf` u = String "App.net"
                  | "twitter.com"   `isInfixOf` u = String "Twitter"
                  | "facebook.com"  `isInfixOf` u = String "Facebook"
                  | "instagram.com" `isInfixOf` u = String "Instagram"
                  | otherwise                     = String u

formatTimeText ∷ Stringable α ⇒ UTCTime → α
formatTimeText = fromString . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

formatTimeAttr ∷ Stringable α ⇒ UTCTime → α
formatTimeAttr = fromString . formatTime defaultTimeLocale "%Y-%m-%dT%H:%MZ"
