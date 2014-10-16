{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll.App (mkApp, defaultSweetrollConf) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import           Text.Pandoc (readMarkdown, def)
import           Web.Simple.Templates.Language
import           Web.Scotty.Trans (ActionT)
import           Web.Scotty
import           Gitson
import           Data.Aeson.Types
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Sweetroll.Pages
import           Sweetroll.Conf
import           Sweetroll.Util

-- | Makes the Sweetroll WAI application.
mkApp :: SweetrollConf -> IO Application
mkApp conf = scottyApp $ do
  middleware autohead -- XXX: does not add Content-Length
  middleware $ staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"

  let render = renderWithConf conf

  get "/:category" $ do
    category <- param "category"
    slugs <- liftIO $ listDocumentKeys category
    maybes <- liftIO $ mapM (readEntry category) slugs
    render categoryTemplate $ catView category (fromMaybe [] $ sequence maybes)

  get "/:category/:slug" $ do
    category <- param "category"
    slug <- param "slug"
    entry <- liftIO (readDocumentByName category slug :: IO (Maybe Entry))
    case entry of
      Just e  -> render entryTemplate $ entryView category (slug, e)
      Nothing -> entryNotFound

  post "/micropub" $ do
    h <- param "h"
    allParams <- params
    now <- liftIO getCurrentTime
    let category = decideCategory allParams
        slug = decideSlug allParams now
        save x = liftIO $ transaction "./" $ saveNextDocument category slug x
        save' x = save x >> created [category, slug]
    case asLText h of
      "entry" -> save' $ makeEntry allParams now
      _ -> status badRequest400

  matchAny "/micropub" $ status methodNotAllowed405

type SweetrollAction = ActionT LText IO

getHost :: SweetrollAction LText
getHost = liftM (fromMaybe "localhost") (header "Host")

renderWithConf :: SweetrollConf -> (SweetrollConf -> Template) -> ViewResult -> SweetrollAction ()
renderWithConf conf tplf stuff = html $ fromStrict $ renderTemplate (layoutTemplate conf) mempty ctx
  where ctx = object [
                "content" .= renderTemplate (tplf conf) mempty (tplContext stuff)
              , "website_title" .= siteName conf
              , "meta_title" .= intercalate (titleSeparator conf) (titleParts stuff ++ [siteName conf])
              ]

created :: [String] -> SweetrollAction ()
created urlParts = do
  status created201
  host <- getHost
  setHeader "Location" $ mkUrl host $ map pack urlParts

entryNotFound :: SweetrollAction ()
entryNotFound = status notFound404

readEntry :: String -> String -> IO (Maybe (String, Entry))
readEntry category n = do
  doc <- readDocument category n :: IO (Maybe Entry)
  return $ (\x -> (n, x)) <$> doc

decideCategory :: [Param] -> CategoryName
decideCategory pars =
  case par "name" of
    Just _ -> "articles"
    _ -> case par "in-reply-to" of
      Just _ -> "replies"
      _ -> "notes"
  where par = findByKey pars

decideSlug :: [Param] -> UTCTime -> EntrySlug
decideSlug pars now = unpack $ fromMaybe fallback $ findByKey pars "slug"
  where fallback = slugify $ fromMaybe (formatTimeSlug now) $ findFirstKey pars ["name", "summary", "content"]
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M"

makeEntry :: [Param] -> UTCTime -> Entry
makeEntry pars now = defaultEntry
  { entryName         = par "name"
  , entrySummary      = par "summary"
  , entryContent      = Left <$> readMarkdown def <$> unpack <$> par "content"
  , entryPublished    = Just $ fromMaybe now $ parseISOTime =<< par "published"
  , entryUpdated      = Just now
  , entryAuthor       = somewhereFromMaybe $ par "author"
  , entryCategory     = parseTags $ fromMaybe "" $ par "category"
  , entryInReplyTo    = Right <$> par "in-reply-to"
  , entryLikeOf       = Right <$> par "like-of"
  , entryRepostOf     = Right <$> par "repost-of" }
  where par = findByKey pars
