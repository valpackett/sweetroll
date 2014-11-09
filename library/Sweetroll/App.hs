{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE PackageImports, ImplicitParams #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll.App (mkApp) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import           Network.HTTP.Link
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import "crypto-random" Crypto.Random
import           Text.Pandoc hiding (Link)
import           Text.Highlighting.Kate.Format.HTML (styleToCss)
import           Web.Scotty
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Data.Stringable
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
  httpClientMgr <- liftIO $ newManager tlsManagerSettings
  sysRandom <- liftIO $ cprgCreate <$> createEntropyPool

  let ?httpMgr = httpClientMgr
      ?rng = sysRandom
      ?conf = conf

  let base = baseUrl conf
      checkAuth' = if testMode conf then id else checkAuth unauthorized
      links = [ Link (mkUrl base ["micropub"])           [(Rel, "micropub")]
              , Link (mkUrl base ["login"])              [(Rel, "token_endpoint")]
              , Link (pack $ indieAuthEndpoint conf)     [(Rel, "authorization_endpoint")] ]
      addLinks l x = addHeader "Link" (fromStrict $ writeLinkHeader l) >> x

  let ?hostInfo = [ "domain" .= domainName conf
                  , "s" .= s conf
                  , "base_url" .= base ]
  let ?authorHtml = renderRaw (authorTemplate conf) ?hostInfo
  let pageNotFound = status notFound404 >> render notFoundTemplate notFoundView

  middleware autohead -- XXX: does not add Content-Length
  middleware $ staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"

  get "/default-style.css" $ setHeader "Content-Type" "text/css" >> raw (defaultStyle conf ++
    (toLazyByteString $ styleToCss $ writerHighlightStyle pandocWriterOptions))

  post "/login" $ doIndieAuth unauthorized

  get "/micropub" $ checkAuth' $ showSyndication $ showAuth

  post "/micropub" $ checkAuth' $ do
    h <- param "h"
    allParams <- params
    now <- liftIO getCurrentTime
    let category = decideCategory allParams
        slug = decideSlug allParams now
        readerF = decideReader allParams
        absUrl = fromStrict $ mkUrl base $ map pack [category, slug]
        save x = (transaction "./" $ saveNextDocument category slug x) >> created absUrl
    case asLText h of
      "entry" -> do
        let entry = makeEntry allParams now absUrl readerF
            ifNotTest x = if testMode conf then (return Nothing) else x
            ifSyndicateTo x y = if isInfixOf x $ fromMaybe "" $ findByKey allParams "syndicate-to" then y else (return Nothing)
        synd <- sequence [ ifNotTest $ ifSyndicateTo "app.net"     $ postAppDotNet entry
                         , ifNotTest $ ifSyndicateTo "twitter.com" $ postTwitter entry ]
        let entry' = entry { entrySyndication = catMaybes synd }
        save entry'
        when (not $ testMode conf) $ void $ sendWebmentions entry'
      _ -> status badRequest400

  get "/" $ addLinks links $ do
    cats <- listCollections >>= mapM readCategory
    render indexTemplate $ indexView $ filter visibleCat cats

  get "/:category" $ addLinks links $ do
    catName <- param "category"
    cat <- readCategory catName
    if null (snd cat) then pageNotFound
    else render categoryTemplate $ catView catName $ snd cat

  get "/:category/:slug" $ addLinks links $ do
    category <- param "category"
    slug <- param "slug"
    entry <- readDocumentByName category slug :: SweetrollAction (Maybe Entry)
    case entry of
      Nothing -> pageNotFound
      Just e  -> do
        otherSlugs <- listDocumentKeys category
        render entryTemplate $ entryView category (map readSlug otherSlugs) (slug, e)

  notFound pageNotFound

created :: LText -> SweetrollAction ()
created url = status created201 >> setHeader "Location" url

unauthorized :: SweetrollAction ()
unauthorized = status unauthorized401

visibleCat :: (CategoryName, [(EntrySlug, Entry)]) -> Bool
visibleCat (slug, entries) = not (null entries)
                          && slug /= "templates"
                          && slug /= "static"

readSlug :: String -> EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry :: CategoryName -> String -> SweetrollAction (Maybe (EntrySlug, Entry))
readEntry category fname = do
  doc <- readDocument category fname :: SweetrollAction (Maybe Entry)
  return $ (\x -> (readSlug fname, x)) <$> doc

readCategory :: CategoryName -> SweetrollAction (CategoryName, [(EntrySlug, Entry)])
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
