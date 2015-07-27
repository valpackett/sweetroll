{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Data.Maybe (fromJust)
import           Data.Microformats2
import           Data.Microformats2.Aeson ()
import           Data.ByteString.Conversion
import qualified Data.Stringable as S
import qualified Network.HTTP.Link as L
import           Network.URI
import           Network.Wai
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Servant
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Auth
import           Sweetroll.Pages
import           Sweetroll.Pagination
import           Sweetroll.Micropub
import           Sweetroll.Syndication (getSyndication)
import           Sweetroll.Style

type WithLink α               = (Headers '[Header "Link" [L.Link]] α)

type LoginRoute'              = "login" :> Post '[FormUrlEncoded] [(Text, Text)]
type LoginRoute               = QueryParam "me" Text :> QueryParam "code" Text :> QueryParam "redirect_uri" Text :> QueryParam "client_id" Text :> QueryParam "state" Text :> LoginRoute'
type PostMicropubRoute        = "micropub" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> AuthProtected :> Post '[FormUrlEncoded] (Headers '[Header "Location" Text] [(Text, Text)])
type GetMicropubRoute         = "micropub" :> QueryParam "q" Text :> AuthProtected :> Get '[FormUrlEncoded] [(Text, Text)]
type IndieConfigRoute         = "indie-config" :> Get '[HTML] IndieConfig
type DefaultCssRoute          = "default-style.css" :> Get '[CSS] LByteString
type CatRoute                 = Capture "catName" String :> QueryParam "page" Int :> Get '[HTML] (WithLink Text)
type EntryRoute               = Capture "catName" String :> Capture "slug" String :> Get '[HTML] (WithLink Text)
type IndexRoute               = Get '[HTML] (WithLink Text)

type SweetrollAPI             = LoginRoute :<|> PostMicropubRoute :<|> GetMicropubRoute :<|> IndieConfigRoute
                           :<|> DefaultCssRoute :<|> CatRoute :<|> EntryRoute :<|> IndexRoute

getMicropub ∷ Maybe Text → JWT VerifiedJWT → Sweetroll [(Text, Text)]
getMicropub (Just "syndicate-to") _ = getSyndication
getMicropub _ token = getAuth token

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return allCss

getIndex ∷ Sweetroll (WithLink Text)
getIndex = do
  ipp ← getConfOpt itemsPerPage
  cats ← listCollections >>= mapM (readCategory ipp (-1))
  selfLink  ← lnk "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy IndexRoute)
  addLinks [selfLink] $ renderIndex $ map (second fromJust) $ filter visibleCat cats

getCat ∷ String → Maybe Int → Sweetroll (WithLink Text)
getCat catName page = do
  let page' = fromMaybe (1) page
  ipp ← getConfOpt itemsPerPage
  cat ← readCategory ipp page' catName
  case snd cat of
    Nothing → throwError err404
    Just p → do
      selfLink ← lnk "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy CatRoute) catName page'
      addLinks [selfLink] $ renderCat catName p

getEntry ∷ String → String → Sweetroll (WithLink Text)
getEntry catName slug = do
  entry ← readDocumentByName catName slug ∷ Sweetroll (Maybe Entry)
  case entry of
    Nothing → throwError err404
    Just e  → do -- cacheHTTPDate (maximumMay $ entryUpdated e) $ do
      otherSlugs ← listDocumentKeys catName
      selfLink ← lnk "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy EntryRoute) catName slug
      addLinks [selfLink] $ renderEntry catName (map readSlug $ sort otherSlugs) (slug, e)

sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT =
       postLogin :<|> postMicropub :<|> getMicropub :<|> getIndieConfig :<|> getDefaultCss
  :<|> getCat :<|> getEntry :<|> getIndex

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx = foldr ($) (sweetrollApp' ctx) [
                     staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
                   , autohead]
  where sweetrollApp' ∷ SweetrollCtx → Application
        sweetrollApp' = serve sweetrollAPI . sweetrollServer
        sweetrollServer ∷ SweetrollCtx → Server SweetrollAPI
        sweetrollServer c = enter (sweetrollToEither c) sweetrollServerT

initSweetrollApp ∷ SweetrollConf → SweetrollTemplates → SweetrollSecrets → IO Application
initSweetrollApp conf tpls secs = initCtx conf tpls secs >>= return . sweetrollApp



instance ToByteString [L.Link] where
  builder = builder . L.writeLinkHeader

lnk ∷ MonadSweetroll μ ⇒ Text → URI → μ L.Link
lnk rel u = do
  conf ← getConf
  let proto = if httpsWorks conf then "https:" else "http:"
      base = URI proto (Just $ URIAuth "" (S.toString $ domainName conf) "") "" "" ""
  return $ L.Link (S.toText . show $ u `relativeTo` base) [(L.Rel, rel)]

addLinks ∷ (MonadSweetroll μ, AddHeader "Link" [L.Link] α β) ⇒ [L.Link] → μ α → μ β
addLinks ls a = do
  conf ← getConf
  micropub ← lnk "micropub" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostMicropubRoute)
  tokenEndpoint ← lnk "token_endpoint" $ safeLink sweetrollAPI (Proxy ∷ Proxy LoginRoute')
  let authorizationEndpoint = L.Link (S.toText $ indieAuthRedirEndpoint conf) [(L.Rel, "authorization_endpoint")]
      hub = L.Link (S.toText $ pushHub conf) [(L.Rel, "hub")]
  return . addHeader (micropub : tokenEndpoint : authorizationEndpoint : hub : ls) =<< a



readSlug ∷ String → EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry ∷ MonadIO μ ⇒ CategoryName → String → μ (Maybe (EntrySlug, Entry))
readEntry category fname = liftIO $ do
  doc ← readDocument category fname ∷ IO (Maybe Entry)
  return $ (\x → (readSlug fname, x)) <$> doc

readCategory ∷ MonadIO μ ⇒ Int → Int → CategoryName → μ (CategoryName, Maybe (Page (EntrySlug, Entry)))
readCategory perPage pageNumber c = liftIO $ do
  slugs ← listDocumentKeys c
  case paginate True perPage pageNumber $ sort slugs of
    Nothing → return (c, Nothing)
    Just page → do
      maybes ← mapM (readEntry c) $ items page
      return (c, Just . changeItems page . fromMaybe [] . sequence $ maybes)

visibleCat ∷ (CategoryName, Maybe (Page (EntrySlug, Entry))) → Bool
visibleCat (slug, Just cat) = (not . null $ items cat)
                              && slug /= "templates"
                              && slug /= "static"
visibleCat (_, Nothing) = False
