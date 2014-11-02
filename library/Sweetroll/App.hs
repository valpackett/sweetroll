{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll.App (mkApp, defaultSweetrollConf) where

import           ClassyPrelude
import           Network.Wai (Application)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.Pandoc (readMarkdown, def)
import           Web.Simple.Templates.Language
import           Web.Scotty.Trans (ActionT)
import           Web.Scotty
import qualified Web.JWT as J
import           Web.JWT (Secret)
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Data.Time.Clock.POSIX
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

  httpClientMgr <- liftIO $ newManager tlsManagerSettings
  indieAuthReq <- liftIO $ parseUrl (indieAuthEndpoint conf) >>= \x -> return $ x { method = "POST", secure = True }
  let httpReq x = liftIO $ httpLbs x httpClientMgr
      secretKey' = J.secret $ secretKey conf

  let s = asText $ if httpsWorks conf then "s" else ""
      baseURL = mconcat ["http", s, "://", domainName conf]
      hostInfo = [ "domain" .= domainName conf
                 , "s" .= s
                 , "base_url" .= baseURL ]
      authorHtml = renderTemplate (authorTemplate conf) mempty (object hostInfo)
      render = renderWithConf conf authorHtml hostInfo

  post "/login" $ do
    code <- param "code"
    me <- param "me"
    redirect_uri <- param "redirect_uri"
    client_id <- param "client_id"
    state <- param "state"
    let valid = makeAccessToken (domainName conf) secretKey' me
    if testMode conf then valid else do
      let reqBody = encodeUtf8 $ mconcat ["code=", code, "&redirect_uri=", redirect_uri, "&client_id=", baseURL, "&state=", state, "&client_id=", client_id]
      resp <- httpReq $ indieAuthReq { requestBody = RequestBodyBS reqBody }
      if responseStatus resp /= ok200 then unauthorized
      else valid

  get "/micropub" $ checkAuth secretKey' $ do
    status ok200
    text $ "me=" ++ fromStrict baseURL
    setHeader "Content-Type" "application/x-www-form-urlencoded; charset=utf-8"

  post "/micropub" $ checkAuth secretKey' $ do
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

  get "/" $ do
    catNames <- liftIO listCollections
    cats <- liftIO $ mapM readCategory catNames
    render indexTemplate $ indexView $ filter visibleCat cats

  get "/:category" $ do
    catName <- param "category"
    cat <- liftIO $ readCategory catName
    render categoryTemplate $ catView catName $ snd cat

  get "/:category/:slug" $ do
    category <- param "category"
    slug <- param "slug"
    entry <- liftIO (readDocumentByName category slug :: IO (Maybe Entry))
    case entry of
      Nothing -> entryNotFound
      Just e  -> do
        otherSlugs <- liftIO $ listDocumentKeys category
        render entryTemplate $ entryView category (map readSlug otherSlugs) (slug, e)

type SweetrollAction = ActionT LText IO

getHost :: SweetrollAction LText
getHost = liftM (fromMaybe "localhost") (header "Host")

renderWithConf :: SweetrollConf -> Text -> [Pair] -> (SweetrollConf -> Template) -> ViewResult -> SweetrollAction ()
renderWithConf conf authorHtml hostInfo tplf stuff = html $ fromStrict $ renderTemplate (layoutTemplate conf) mempty ctx
  where ctx = object $ hostInfo ++ [
                "content" .= renderTemplate (tplf conf) mempty (tplContext stuff)
              , "author" .= authorHtml
              , "website_title" .= siteName conf
              , "meta_title" .= intercalate (titleSeparator conf) (titleParts stuff ++ [siteName conf])
              ]

created :: [String] -> SweetrollAction ()
created urlParts = do
  status created201
  hostH <- getHost
  setHeader "Location" $ mkUrl hostH $ map pack urlParts

entryNotFound :: SweetrollAction ()
entryNotFound = status notFound404

unauthorized :: SweetrollAction ()
unauthorized = status unauthorized401

checkAuth :: Secret -> SweetrollAction () -> SweetrollAction ()
checkAuth secKey act = do
  allParams <- params
  tokenHeader <- header "Authorization" >>= \x -> return $ fromMaybe "" $ drop 7 <$> x -- Drop "Bearer "
  let token = toStrict $ fromMaybe tokenHeader $ findByKey allParams "access_token"
      verResult = J.decodeAndVerifySignature secKey token
  case verResult of
    Just _ -> act
    _ -> unauthorized

makeAccessToken :: Text -> Secret -> Text -> SweetrollAction ()
makeAccessToken issuer key me = do
  now <- liftIO getCurrentTime
  let claims = J.def { J.iss = J.stringOrURI issuer
                     , J.sub = J.stringOrURI me
                     , J.iat = J.intDate $ utcTimeToPOSIXSeconds now }
  status ok200
  text $ fromStrict $ mconcat ["access_token=", J.encodeSigned J.HS256 key claims, "&scope=post&me=", me]
  setHeader "Content-Type" "application/jwt"

visibleCat :: (CategoryName, [(EntrySlug, Entry)]) -> Bool
visibleCat (slug, entries) =
     not (null entries)
  && slug /= "templates"

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
