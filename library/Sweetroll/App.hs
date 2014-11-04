{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll.App (mkApp, defaultSweetrollConf) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import           Network.HTTP.Link
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.Pandoc hiding (Link)
import           Web.Scotty
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Data.Aeson.Types
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Sweetroll.Pages
import           Sweetroll.Auth
import           Sweetroll.Syndication
import           Sweetroll.Webmention
import           Sweetroll.Conf
import           Sweetroll.Util

-- | Makes the Sweetroll WAI application.
mkApp :: SweetrollConf -> IO Application
mkApp conf = scottyApp $ do
  middleware autohead -- XXX: does not add Content-Length
  middleware $ staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"

  httpClientMgr <- liftIO $ newManager tlsManagerSettings

  let base = baseUrl conf
      hostInfo = [ "domain" .= domainName conf
                 , "s" .= s conf
                 , "base_url" .= base ]
      authorHtml = renderRaw (authorTemplate conf) hostInfo
      render = renderWithConf conf authorHtml hostInfo
      checkAuth' = if testMode conf then id else checkAuth conf unauthorized
      links = [ Link (mkUrl base ["micropub"])           [(Rel, "micropub")]
              , Link (mkUrl base ["login"])              [(Rel, "token_endpoint")]
              , Link "https://indieauth.com/auth"        [(Rel, "authorization_endpoint")] ]
      addLinks l x = addHeader "Link" (fromStrict $ writeLinkHeader l) >> x

  post "/login" $ doIndieAuth conf unauthorized httpClientMgr

  get "/micropub" $ checkAuth' $ showSyndication $ showAuth

  post "/micropub" $ checkAuth' $ do
    h <- param "h"
    allParams <- params
    now <- liftIO getCurrentTime
    let category = decideCategory allParams
        slug = decideSlug allParams now
        readerF = decideReader allParams
        absUrl = fromStrict $ mkUrl base $ map pack [category, slug]
        save x = liftIO $ transaction "./" $ saveNextDocument category slug x
        save' x = save x >> created absUrl
    case asLText h of
      "entry" -> do
        let entry = makeEntry allParams now absUrl readerF
            ifNotTest x = if testMode conf then (return Nothing) else x
            ifSyndicateTo x y = if isInfixOf x $ fromMaybe "" $ findByKey allParams "syndicate-to" then y else (return Nothing)
        adnPost <- ifNotTest $ ifSyndicateTo "app.net" $ postAppDotNet conf httpClientMgr entry
        let entry' = entry { entrySyndication = catMaybes [adnPost] }
        save' entry'
        when (not $ testMode conf) $ void $ liftIO $ sendWebmentions httpClientMgr entry'
      _ -> status badRequest400

  get "/" $ addLinks links $ do
    catNames <- liftIO listCollections
    cats <- liftIO $ mapM readCategory catNames
    render indexTemplate $ indexView $ filter visibleCat cats

  get "/:category" $ addLinks links $ do
    catName <- param "category"
    cat <- liftIO $ readCategory catName
    render categoryTemplate $ catView catName $ snd cat

  get "/:category/:slug" $ addLinks links $ do
    category <- param "category"
    slug <- param "slug"
    entry <- liftIO (readDocumentByName category slug :: IO (Maybe Entry))
    case entry of
      Nothing -> entryNotFound
      Just e  -> do
        otherSlugs <- liftIO $ listDocumentKeys category
        render entryTemplate $ entryView category (map readSlug otherSlugs) (slug, e)

created :: LText -> SweetrollAction ()
created url = status created201 >> setHeader "Location" url

entryNotFound :: SweetrollAction ()
entryNotFound = status notFound404

unauthorized :: SweetrollAction ()
unauthorized = status unauthorized401

visibleCat :: (CategoryName, [(EntrySlug, Entry)]) -> Bool
visibleCat (slug, entries) = not (null entries)
                          && slug /= "templates"
                          && slug /= "static"

readSlug :: String -> EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry :: CategoryName -> String -> IO (Maybe (EntrySlug, Entry))
readEntry category fname = do
  doc <- readDocument category fname :: IO (Maybe Entry)
  return $ (\x -> (readSlug fname, x)) <$> doc

readCategory :: CategoryName -> IO (CategoryName, [(EntrySlug, Entry)])
readCategory c = do
  slugs <- listDocumentKeys c
  maybes <- mapM (readEntry c) slugs
  return (c, reverse $ fromMaybe [] $ sequence maybes)

decideCategory :: [Param] -> CategoryName
decideCategory pars | isJust $ par "name"          = "articles"
                    | isJust $ par "in-reply-to"   = "replies"
                    | isJust $ par "like-of"       = "likes"
                    | otherwise                    = "notes"
  where par = findByKey pars

decideSlug :: [Param] -> UTCTime -> EntrySlug
decideSlug pars now = unpack $ fromMaybe fallback $ findByKey pars "slug"
  where fallback = slugify $ fromMaybe (formatTimeSlug now) $ findFirstKey pars ["name", "summary"]
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

decideReader :: [Param] -> (ReaderOptions -> String -> Pandoc)
decideReader pars | f == Just "textile"     = readTextile
                  | f == Just "org"         = readOrg
                  | f == Just "rst"         = readRST
                  | f == Just "html"        = readHtml
                  | f == Just "latex"       = readLaTeX
                  | f == Just "tex"         = readLaTeX
                  | otherwise               = readMarkdown
  where f = findByKey pars "format"

makeEntry :: [Param] -> UTCTime -> LText -> (ReaderOptions -> String -> Pandoc) -> Entry
makeEntry pars now absUrl readerF = defaultEntry
  { entryName         = par "name"
  , entrySummary      = par "summary"
  , entryContent      = Left <$> readerF pandocReaderOptions <$> unpack <$> par "content"
  , entryPublished    = Just $ fromMaybe now $ parseISOTime =<< par "published"
  , entryUpdated      = Just now
  , entryAuthor       = somewhereFromMaybe $ par "author"
  , entryCategory     = parseTags $ fromMaybe "" $ par "category"
  , entryUrl          = Just absUrl
  , entryInReplyTo    = Right <$> par "in-reply-to"
  , entryLikeOf       = Right <$> par "like-of"
  , entryRepostOf     = Right <$> par "repost-of" }
  where par = findByKey pars
