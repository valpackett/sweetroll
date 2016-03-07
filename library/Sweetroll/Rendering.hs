{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DataKinds #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Rendering where

import           ClassyPrelude hiding (fromString)
import           Control.Lens hiding (Index, re, parts, (.=))
import           Network.HTTP.Media.MediaType
import           Data.Aeson (encode)
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.List (elemIndex)
import           Data.Foldable (asum)
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import           Safe (atMay)
import           Network.URI (nullURI)
import           Network.HTTP.Types
import           Servant
import           Sweetroll.Pages
import           Sweetroll.Slice
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
  mimeRender _ = cs

instance Templatable α ⇒ MimeRender HTML (View α) where
  mimeRender x v@(View conf renderer _) = mimeRender x $ renderer (templateName v) (withMeta conf $ context v)

withMeta ∷ ToJSON α ⇒ SweetrollConf → α → Value
withMeta conf d =
  object [ "meta" .= object [ "base_uri" .= toLT (show $ fromMaybe nullURI $ baseURI conf)
                            , "site_name" .= siteName conf ]
         , "data" .= d ]

instance Accept CSS where
  contentType _ = "text" // "css"

instance ConvertibleStrings α LByteString ⇒ MimeRender CSS α where
  mimeRender _ = cs

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
            , "titleParts"      .= [ toLT titleName, toLT catName ] ]
          slugIdx = fromMaybe (-1) $ elemIndex slug otherSlugs
          prev = atMay otherSlugs $ slugIdx - 1
          next = atMay otherSlugs $ slugIdx + 1
          titleName = orEmptyMaybe $ asum [ e ^? key "properties" . key "name" . nth 0 . _String
                                          , e ^? key "properties" . key "published" . nth 0 . _String ]

instance Templatable IndexedPage where
  templateName _ = "index"
  context (View _ _ (IndexedPage catNames slices entries)) = ctx
    where ctx = object [ "categoryNames" .= catNames
                       , "slices" .= object (map (\s → toST (sliceCatName s) .= sliceContext s) slices)
                       , "entries" .= entries ]
          sliceContext slice = object [
              "name"            .= sliceCatName slice
            , "permalink"       .= showLink (catLink slice)
            , "entryUrls"       .= map snd (sliceItems slice)
            , "hasBefore"       .= isJust (sliceBefore slice)
            , "beforeHref"      .= orEmptyMaybe (showLink . permalink (Proxy ∷ Proxy CatRouteB) (sliceCatName slice) <$> sliceBefore slice)
            , "hasAfter"        .= isJust (sliceAfter slice)
            , "afterHref"       .= orEmptyMaybe (showLink . permalink (Proxy ∷ Proxy CatRouteA) (sliceCatName slice) <$> sliceAfter slice)
            , "titleParts"      .= [ toLT $ sliceCatName slice ] ]


renderError ∷ MonadSweetroll μ ⇒ ServantErr → ByteString → μ ServantErr
renderError origErr tplName = do
  renderer ← getRenderer
  conf ← getConf
  return $ origErr { errHeaders = (hContentType, "text/html; charset=utf-8") : errHeaders origErr
                   , errBody = cs $ renderer tplName (withMeta conf $ object []) }
