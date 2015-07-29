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
import qualified Data.Stringable as S
import qualified Network.HTTP.Link as L
import           Network.URI
import           Network.Wai
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Routed
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
import           Sweetroll.Util (serveStaticFromLookup)

type WithLink α               = (Headers '[Header "Link" [L.Link]] α)

type LoginRoute               = "login" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] [(Text, Text)]
type IndieConfigRoute         = "indie-config" :> Get '[HTML] IndieConfig
type DefaultCssRoute          = "default-style.css" :> Get '[CSS] LByteString
type EntryRoute               = Capture "catName" String :> Capture "slug" String :> Get '[HTML] (WithLink Text)
type CatRoute                 = Capture "catName" String :> QueryParam "page" Int :> Get '[HTML] (WithLink Text)
type IndexRoute               = Get '[HTML] (WithLink Text)

type PostMicropubRoute        = "micropub" :> AuthProtect :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] (Headers '[Header "Location" Text] [(Text, Text)])
type GetMicropubRoute         = "micropub" :> AuthProtect :> QueryParam "q" Text :> Get '[FormUrlEncoded] [(Text, Text)]

type SweetrollAPI             = LoginRoute :<|> IndieConfigRoute :<|> DefaultCssRoute
                           :<|> EntryRoute :<|> CatRoute :<|> IndexRoute
                           :<|> PostMicropubRoute :<|> GetMicropubRoute

getMicropub ∷ JWT VerifiedJWT → Maybe Text → Sweetroll [(Text, Text)]
getMicropub _ (Just "syndicate-to") = getSyndication
getMicropub token _ = getAuth token

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return allCss

getIndex ∷ Sweetroll (WithLink Text)
getIndex = do
  ipp ← getConfOpt itemsPerPage
  cats ← listCollections >>= mapM (readCategory ipp (-1))
  selfLink  ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy IndexRoute)
  addLinks [selfLink] $ renderIndex $ map (second fromJust) $ filter visibleCat cats

getCat ∷ String → Maybe Int → Sweetroll (WithLink Text)
getCat catName page = do
  let page' = fromMaybe (-1) page
  ipp ← getConfOpt itemsPerPage
  cat ← readCategory ipp page' catName
  case snd cat of
    Nothing → throwError err404
    Just p → do
      selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy CatRoute) catName page'
      addLinks [selfLink] $ renderCat catName p

getEntry ∷ String → String → Sweetroll (WithLink Text)
getEntry catName slug = do
  entry ← readDocumentByName catName slug ∷ Sweetroll (Maybe Entry)
  case entry of
    Nothing → throwError err404
    Just e  → do -- cacheHTTPDate (maximumMay $ entryUpdated e) $ do
      otherSlugs ← listDocumentKeys catName
      selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy EntryRoute) catName slug
      addLinks [selfLink] $ renderEntry catName (map readSlug $ sort otherSlugs) (slug, e)

sweetrollServerT ∷ SweetrollCtx → ServerT SweetrollAPI Sweetroll
sweetrollServerT ctx = postLogin :<|> getIndieConfig :<|> getDefaultCss :<|> getEntry :<|> getCat :<|> getIndex
                  :<|> AuthProtected key postMicropub :<|> AuthProtected key getMicropub
    where key = secretKey $ _ctxSecs ctx

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx = foldr ($) (sweetrollApp' ctx) [
                     staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
                   , routedMiddleware ((== (Just "bower")) . headMay) $ serveStaticFromLookup bowerComponents
                   , autohead]
  where sweetrollApp' ∷ SweetrollCtx → Application
        sweetrollApp' = serve sweetrollAPI . sweetrollServer
        sweetrollServer ∷ SweetrollCtx → Server SweetrollAPI
        sweetrollServer c = enter (sweetrollToEither c) $ sweetrollServerT c

initSweetrollApp ∷ SweetrollConf → SweetrollTemplates → SweetrollSecrets → IO Application
initSweetrollApp conf tpls secs = initCtx conf tpls secs >>= return . sweetrollApp


genLink ∷ MonadSweetroll μ ⇒ Text → URI → μ L.Link
genLink rel u = do
  conf ← getConf
  let proto = if httpsWorks conf then "https:" else "http:"
      base = URI proto (Just $ URIAuth "" (S.toString $ domainName conf) "") "" "" ""
  return $ L.Link (u `relativeTo` base) [(L.Rel, rel)]

addLinks ∷ (MonadSweetroll μ, AddHeader "Link" [L.Link] α β) ⇒ [L.Link] → μ α → μ β
addLinks ls a = do
  conf ← getConf
  micropub ← genLink "micropub" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostMicropubRoute)
  tokenEndpoint ← genLink "token_endpoint" $ safeLink sweetrollAPI (Proxy ∷ Proxy LoginRoute)
  let authorizationEndpoint = fromJust $ L.lnk (indieAuthRedirEndpoint conf) [(L.Rel, "authorization_endpoint")]
      hub = fromJust $ L.lnk (pushHub conf) [(L.Rel, "hub")]
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
