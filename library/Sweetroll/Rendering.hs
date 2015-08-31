{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DataKinds #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Rendering where

import           ClassyPrelude hiding (fromString)
import           Control.Lens hiding (Index, re, parts, (.=))
import           Network.HTTP.Media.MediaType
import           Data.Aeson (encode)
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.List (elemIndex)
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Stringable
import           Safe (atMay)
import           Servant hiding (toText)
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
  mimeRender x v@(View conf renderer _) = mimeRender x $ renderer (templateName v) (withMeta $ context v)
    where withMeta d =
            object [ "meta" .= object [ "base_uri" .= toText (show $ baseURI conf)
                                      , "site_name" .= siteName conf ]
                   , "data" .= d ]

instance Accept CSS where
  contentType _ = "text" // "css"

instance Stringable α ⇒ MimeRender CSS α where
  mimeRender _ = toLazyByteString

view ∷ α → Sweetroll (View α)
view content = do
  conf ← getConf
  renderer ← getRenderer
  return $ View conf renderer content

class Templatable α where
  templateName ∷ View α → ByteString
  context ∷ View α → Value

instance Templatable EntryPage where
  templateName _ = "entry"
  context (View _ _ (EntryPage catName otherSlugs (slug, e))) = ctx
    where ctx = object [
              "entry"            .= e
            , "permalink"        .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack slug)
            , "categoryName"     .= catName
            , "categoryHref"     .= showLink (permalink (Proxy ∷ Proxy CatRouteE) catName)
            , "hasPrev"          .= isJust prev
            , "prevHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe prev)
            , "hasNext"          .= isJust next
            , "nextHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe next)
            , "hasTwitterId"     .= isJust twitterId
            , "twitterId"        .= orEmptyMaybe twitterId ]
          slugIdx = fromMaybe (-1) $ elemIndex slug otherSlugs
          prev = atMay otherSlugs $ slugIdx - 1
          next = atMay otherSlugs $ slugIdx + 1
          twitterId = lastMay =<< T.splitOn "/" <$> find ("twitter.com" `isInfixOf`) entrySyndication
          entrySyndication = mapMaybe (^? _String) $ V.toList $ fromMaybe V.empty $ e ^? key "properties" . key "syndication" . _Array

instance Templatable CatPage where
  templateName _ = "category"
  context (View conf renderer (CatPage name slice)) = ctx
    where ctx = object [
              "name"            .= name
            , "permalink"       .= showLink (sliceSelf slice)
            , "entries"         .= map entryContext entries
            , "hasBefore"       .= isJust (sliceBefore slice)
            , "beforeHref"      .= orEmptyMaybe (showLink <$> sliceBefore slice)
            , "hasAfter"        .= isJust (sliceAfter slice)
            , "afterHref"       .= orEmptyMaybe (showLink <$> sliceAfter slice) ]
          entryContext = context . View conf renderer . EntryPage name slugs
          slugs = map fst entries
          entries = sliceItems slice

instance Templatable IndexPage where
  templateName _ = "index"
  context (View conf renderer (IndexPage cats)) = ctx
    where ctx = object [ "categories" .= map catContext (sortOn (fromMaybe 999 . flip elemIndex (categoryOrder conf) . fst) cats) ]
          catContext (n, p) = context (View conf renderer (CatPage n p))
