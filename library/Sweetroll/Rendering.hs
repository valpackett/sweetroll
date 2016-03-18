{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DataKinds #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Rendering where

import           Sweetroll.Prelude hiding (fromString)
import           Network.HTTP.Media.MediaType
import           Data.List (elemIndex)
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsToAtom
import           Text.XML.Writer as W
import qualified Text.HTML.DOM as HTML
import           Text.XML.Lens (entire, named, text)
import           Text.XML as XML
import           Safe (atMay)
import           Servant
import           Sweetroll.Pages
import           Sweetroll.Slice
import           Sweetroll.Routes
import           Sweetroll.Conf
import           Sweetroll.Monads

instance Accept Atom where
  contentType _ = "application" // "atom+xml" /: ("charset", "utf-8")

instance MimeRender Atom Document where
  mimeRender _ = XML.renderLBS def { rsPretty = False, XML.rsNamespaces = [("atom", "http://www.w3.org/2005/Atom"), ("activity", "http://activitystrea.ms/spec/1.0/")] }

instance MimeRender Atom (View IndexedPage) where
  mimeRender p v@(View conf _ (IndexedPage catNames _ _)) = mimeRender p $ feedToAtom addMetaStuff $ parseMf2 mfSettings root
    where root = documentRoot $ HTML.parseLBS $ mimeRender (Proxy ∷ Proxy HTML) v
          mfSettings = def { baseUri = Just rootUri }
          rootUri = fromMaybe nullURI $ baseURI conf
          altUri = fromMaybe nullURI (parseURIReference $ intercalate "+" catNames) `relativeTo` rootUri
          el x = W.element (Name x (Just "http://www.w3.org/2005/Atom") Nothing)
          elA x = W.elementA (Name x (Just "http://www.w3.org/2005/Atom") Nothing)
          addMetaStuff = do
            el "title" $ content $ fromMaybe "" $ root ^? entire . named "title" . text
            elA "link" [("rel", "alternate"), ("type", "text/html"), ("href", tshow altUri)] W.empty
            elA "link" [("rel", "self"), ("type", "application/atom+xml"), ("href", tshow $ atomizeUri altUri)] W.empty
            fromMaybe W.empty $ (\x → elA "link" [("rel", "hub"), ("href", cs x)] W.empty) <$> pushHub conf

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
  object [ "meta" .= object [ "baseUri" .= toLT (show $ fromMaybe nullURI $ baseURI conf)
                            , "siteName" .= siteName conf
                            , "categoriesInLanding" .= categoriesInLanding conf
                            , "categoriesInNav" .= categoriesInNav conf
                            , "categoryTitles" .= categoryTitles conf ]
         , "data" .= d ]

instance Accept CSS where
  contentType _ = "text" // "css"

instance ConvertibleStrings α LByteString ⇒ MimeRender CSS α where
  mimeRender _ = cs

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
            , "nextHref"         .= showLink (permalink (Proxy ∷ Proxy EntryRoute) catName $ pack $ orEmptyMaybe next) ]
          slugIdx = fromMaybe (-1) $ elemIndex slug otherSlugs
          prev = atMay otherSlugs $ slugIdx - 1
          next = atMay otherSlugs $ slugIdx + 1

instance Templatable IndexedPage where
  templateName _ = "index"
  context (View _ _ (IndexedPage catNames slices entries)) = ctx
    where ctx = object [ "categoriesRequested" .= catNames
                       , "slices" .= object (map (\s → toST (sliceCatName s) .= sliceContext s) slices)
                       , "entries" .= entries ]
          sliceContext slice = object [
              "name"            .= sliceCatName slice
            , "permalink"       .= showLink (catLink slice)
            , "entryUrls"       .= map snd (sliceItems slice)
            , "hasBefore"       .= isJust (sliceBefore slice)
            , "beforeHref"      .= orEmptyMaybe (showLink . permalink (Proxy ∷ Proxy CatRouteB) (sliceCatName slice) <$> sliceBefore slice)
            , "hasAfter"        .= isJust (sliceAfter slice)
            , "afterHref"       .= orEmptyMaybe (showLink . permalink (Proxy ∷ Proxy CatRouteA) (sliceCatName slice) <$> sliceAfter slice) ]


renderError ∷ MonadSweetroll μ ⇒ ServantErr → ByteString → μ ServantErr
renderError origErr tplName = do
  renderer ← getRenderer
  conf ← getConf
  return $ origErr { errHeaders = (hContentType, "text/html; charset=utf-8") : errHeaders origErr
                   , errBody = cs $ renderer tplName (withMeta conf $ object []) }
