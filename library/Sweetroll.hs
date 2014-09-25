{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE CPP, QuasiQuotes #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll (mkApp, defaultSweetrollConf) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.HTTP.Types.Status
import           Text.RawString.QQ
import           Text.Pandoc
import           Web.Scotty.Trans (ActionT)
import           Web.Scotty
import           Gitson
import           Data.Data
import           Data.Microformats2
import           Data.Microformats2.Aeson ()
import           Sweetroll.Pages
import           Sweetroll.Util

-- | Makes the Sweetroll WAI application.
mkApp :: SweetrollConf -> IO Application
mkApp conf = scottyApp $ do
  middleware autohead -- XXX: does it even work properly?

  let render = ok $ pageTemplate conf

  get "/:category" $ do
    category <- param "category"
    slugs <- liftIO $ listDocumentKeys category
    maybes <- liftIO $ mapM (readD' category) slugs
    let cpg = categoryPage (pack category) (fromMaybe [] $ sequence maybes)
    render $ cpg

  get "/:category/:slug" $ do
    category <- param "category"
    slug <- param "slug"
    entry <- liftIO (readDocumentByName category slug :: IO (Maybe Entry))
    case entry of
      Just e  -> render $ entryPage (mconcat ["/", pack category, "/", pack slug]) e
      Nothing -> entryNotFound

  post "/micropub" $ do
    h :: LText <- param "h"
    allParams <- params
    now <- liftIO getCurrentTime
    let findParam = findByKey allParams
        category = case findParam "name" of
          Just _ -> "articles"
          Nothing -> "notes"
        slug = fromMaybe (slugify $ fromMaybe (formatISOTime now) $ findFirstKey allParams ["name", "summary", "content"]) $ findParam "slug"
        save x = liftIO $ transaction "./" $ saveNextDocument category (unpack slug) x
    case h of
      "entry" -> do
        save $ defaultEntry {
              entryName         = findParam "name"
            , entrySummary      = findParam "summary"
            , entryContent      = Left <$> readMarkdown def <$> unpack <$> findParam "content"
            , entryPublished    = Just $ fromMaybe now $ parseISOTime =<< findParam "published"
            , entryUpdated      = Just now
            , entryAuthor       = somewhereFromMaybe $ findParam "author"
            , entryCategory     = parseTags $ fromMaybe "" $ findParam "category"
            , entryInReplyTo    = Right <$> findParam "in-reply-to"
            , entryLikeOf       = Right <$> findParam "like-of"
            , entryRepostOf     = Right <$> findParam "repost-of" }
        created [category, slug]
      _ -> status badRequest400

  matchAny "/micropub" $ status methodNotAllowed405

------------ Actions {{{

type SweetrollAction = ActionT LText IO

getHost :: SweetrollAction LText
getHost = liftM (fromMaybe "localhost") (header "Host")

ok :: Data d => Text -> d -> SweetrollAction ()
ok tpl dat = liftIO (renderPage tpl dat) >>= html

created :: [LText] -> SweetrollAction ()
created urlParts = do
  status created201
  host <- getHost
  setHeader "Location" $ mkUrl host urlParts

entryNotFound :: SweetrollAction ()
entryNotFound = status notFound404

------------ }}}

------------ Helpers {{{

readD' :: String -> String -> IO (Maybe (LText, Entry))
readD' category n = do
  doc <- readDocument category n :: IO (Maybe (Entry))
  return $ (\x -> (pack n, x)) <$> doc

------------ }}}

------------ Configuration {{{

data SweetrollConf = SweetrollConf
  { pageTemplate :: Text }

-- cpp screws up line numbering, so we put this at the end
defaultSweetrollConf :: SweetrollConf
defaultSweetrollConf =  SweetrollConf {
    pageTemplate = dropNonHtml [r|
#include "../templates/page.html"
|] }
------------ }}}
