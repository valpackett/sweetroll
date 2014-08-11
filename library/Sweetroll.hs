{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll (app) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans (ActionT)
import           Web.Scotty
import           Gitson
import           Sweetroll.Types
import           Sweetroll.Util

getHost :: ActionT LText IO LText
getHost = liftM (fromMaybe "localhost") (header "Host")

created :: [LText] -> ActionT LText IO ()
created urlParts = do
  status created201
  host <- getHost
  setHeader "Location" $ mconcat $ ["http://", host, "/"] ++ urlParts

-- | The Sweetroll WAI application.
app :: IO Application
app = scottyApp $ do
  middleware autohead -- XXX: does it even work properly?

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
        created [category, "/", slug]
      _ -> status badRequest400

  matchAny "/micropub" $ status methodNotAllowed405
