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
import           Web.Scotty.Trans (ActionT)
import           Web.Scotty
import           Gitson
import           Sweetroll.Types
import           Sweetroll.Pages
import           Sweetroll.Util

-- | Makes the Sweetroll WAI application.
mkApp :: SweetrollConf -> IO Application
mkApp conf = scottyApp $ do
  middleware autohead -- XXX: does it even work properly?

  let render = ok $ pageTemplate conf

  get "/:category/:slug" $ do
    category <- param "category"
    slug <- param "slug"
    entry <- liftIO $ (readEntryByName category slug :: IO (Maybe Entry))
    case entry of
      Just e  -> render $ entryPage e
      Nothing -> entryNotFound

  post "/micropub" $ do
    h :: LText <- param "h"
    allParams <- params
    now <- liftIO getCurrentTime
    let findParam = findByKey allParams
        category = fromMaybe "notes" $ findParam "category"
        slug = fromMaybe (slugify $ fromMaybe (formatISOTime now) $ findFirstKey allParams ["name", "summary", "content"]) $ findParam "slug"
        save x = liftIO $ transaction "./" $ saveNextEntry (unpack category) (unpack slug) x
    case h of
      "entry" -> do
        save Entry {
              entryName      = findParam "name"
            , entrySummary   = findParam "summary"
            , entryContent   = findParam "content"
            , entryPublished = fromMaybe now $ parseISOTime $ findParam "published"
            , entryUpdated   = now
            , entryTags      = parseTags $ fromMaybe "" $ findParam "tags"
            , entryAuthor    = findParam "author"
            , entryInReplyTo = findParam "in-reply-to"
            , entryLikeOf    = findParam "like-of"
            , entryRepostOf  = findParam "repost-of" }
        created [category, slug]
      _ -> status badRequest400

  matchAny "/micropub" $ status methodNotAllowed405

------------ Actions {{{

type SweetrollAction = ActionT LText IO

getHost :: SweetrollAction LText
getHost = liftM (fromMaybe "localhost") (header "Host")

ok :: Text -> Page -> SweetrollAction ()
ok tpl dat = liftIO (renderPage tpl dat) >>= html

created :: [LText] -> SweetrollAction ()
created urlParts = do
  status created201
  host <- getHost
  setHeader "Location" $ mkUrl host urlParts

entryNotFound :: SweetrollAction ()
entryNotFound = status notFound404

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
