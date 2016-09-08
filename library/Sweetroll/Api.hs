{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           Sweetroll.Prelude hiding (Context)
import           Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HMS
import           Data.Microformats2.Parser.HtmlUtil (getInnerHtml)
import qualified Text.HTML.DOM as HTML
import           Text.XML (documentRoot)
import qualified Network.HTTP.Link as L
import           Network.Wai
import           Network.Wai.UrlMap
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Throttle
import           Network.Wai.Application.Static
import           WaiAppStatic.Types
import           Servant
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Pages
import           Sweetroll.Slice
import           Sweetroll.Rendering
import           Sweetroll.Auth
import           Sweetroll.Micropub.Endpoint
import           Sweetroll.Webmention.Receive
import           Sweetroll.Proxy

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getBaseCss ∷ Sweetroll LByteString
getBaseCss = return baseCss

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return defaultCss

getDefaultIcons ∷ Sweetroll LByteString
getDefaultIcons = return defaultIcons

getIndex ∷ Sweetroll (WithLink (View IndexedPage))
getIndex = do
  ipp ← getConfOpt itemsPerPage
  catNames ← getConfOpt categoriesInLanding
  (slices, entries0) ← foldM (\(slices, entries) catName →
                               readCategory ipp Nothing Nothing entries catName >>= \(slices', entries') → return (slices ++ slices', entries'))
                             ([], HMS.empty)
                             catNames
  entries ← postprocessEntries entries0
  selfLink ← genLink "self" $ permalink (Proxy ∷ Proxy IndexRoute)
  addLinks [selfLink] $ mkView $ IndexedPage catNames slices entries

getCat ∷ String → Maybe Int → Maybe Int → Sweetroll (WithLink (View IndexedPage))
getCat catName before after = do
  ipp ← getConfOpt itemsPerPage
  (slice : slices, entries0) ← readCategory ipp before after HMS.empty catName
  entries ← postprocessEntries entries0
  guardBoolM (renderError err404 "404") (not $ null $ sliceItems slice)
  selfLink ← genLink "self" $ catLink slice
  addLinks [selfLink] $ mkView $ IndexedPage [catName] (slice : slices) entries

getEntry ∷ String → String → Sweetroll (WithLink (View EntryPage))
getEntry catName slug = do
  entry ← postprocessEntry =<< (guardJustM (notFoundOrGone catName slug) $ readDocumentByName catName slug)
  -- TODO: cacheHTTPDate -- don't forget responses' dates! -- 204 will be thrown before rendering!
  otherSlugs ← listDocumentKeys catName
  selfLink ← genLink "self" $ permalink (Proxy ∷ Proxy EntryRoute) catName slug
  addLinks [selfLink] $ mkView $ EntryPage catName (map readSlug $ sort otherSlugs) (slug, entry)

postprocessEntries ∷ HashMap α Value → Sweetroll (HashMap α Value)
postprocessEntries = mapM postprocessEntry

postprocessEntry ∷ Value → Sweetroll Value
postprocessEntry entry = do
  secs ← getSecs
  conf ← getConf
  let proxifyLink s = proxiedUri secs s
      proxifyHtml s = fromMaybe s $ getInnerHtml Nothing $
                        (proxyImages secs conf . detwitterizeEmoji) $ documentRoot $ HTML.parseSTChunks ["<div>", s, "</div>"]
      ppr e = foldr ($) e [ (& key "properties" . key "photo" . values . _String %~ proxifyLink)
                          , (& key "properties" . key "content" . values . key "html" . _String %~ proxifyHtml) ]
  return $ transform (\x → if isJust (x ^? key "type") then ppr x else x) entry


sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT = getIndieConfig :<|> getBaseCss :<|> getDefaultCss :<|> getDefaultIcons
                  :<|> postLogin :<|> getAuth
                  :<|> postMedia :<|> postMicropub :<|> getMicropub
                  :<|> receiveWebmention
                  :<|> getEntry :<|> getCat :<|> getIndex

sweetrollApp ∷ WaiThrottle → SweetrollCtx → Application
sweetrollApp thr ctx =
    simpleCors
  $ throttle defThr { isThrottled = (\r → return $ requestMethod r /= "GET" && requestMethod r /= "HEAD")
                    , throttleBurst = 5 } thr
  $ autohead
  $ cacheControlAuto
  $ acceptOverride
  $ gzip def
  $ supportFormAuth
  $ mapUrls $ mount "bower" (staticApp $ (embeddedSettings bowerComponents) { ssMaxAge = MaxAgeSeconds 30 })
          <|> mount "static" (staticApp $ (defaultWebAppSettings "static") { ssMaxAge = MaxAgeSeconds 30 })
          <|> mount "proxy" (requestProxy ctx)
          <|> mountRoot (serveWithContext sweetrollAPI sweetrollContext $ sweetrollServer ctx)
  where sweetrollServer c = enter (sweetrollToExcept c) sweetrollServerT
        sweetrollContext = authHandler (secretKey $ _ctxSecs ctx) :. EmptyContext
        defThr = defaultThrottleSettings

cacheControlAuto ∷ Middleware
cacheControlAuto app req respond = app req $ respond . setCC
  where ver = join $ lookup "_v" $ queryString req
        setCC resp = if isJust ver
                        then mapResponseHeaders (insertMap hCacheControl "public, max-age=31536000, immutable") resp
                        else resp

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = do
  thr ← initThrottler
  fmap (sweetrollApp thr) $ initCtx conf secs


genLink ∷ MonadSweetroll μ ⇒ Text → URI → μ L.Link
genLink rel u = do
  base ← getConfOpt baseURI
  return $ L.Link (u `relativeTo` base) [(L.Rel, rel)]

addLinks ∷ (MonadSweetroll μ, AddHeader "Link" [L.Link] α β) ⇒ [L.Link] → μ α → μ β
addLinks ls a = do
  webmention ← genLink "webmention" $ permalink (Proxy ∷ Proxy PostWebmentionRoute)
  micropub ← genLink "micropub" $ permalink (Proxy ∷ Proxy PostMicropubRoute)
  tokenEndpoint ← genLink "token_endpoint" $ permalink (Proxy ∷ Proxy PostLoginRoute)
  authorizationEndpoint ← getConfOpt indieAuthRedirEndpoint >>= \x → return $ fromJust $ L.lnk x [(L.Rel, "authorization_endpoint")]
  hub ← getConfOpt pushHub >>= \x → return $ fromJust $ L.lnk x [(L.Rel, "hub")]
  return . addHeader (webmention : micropub : tokenEndpoint : authorizationEndpoint : hub : ls) =<< a



readSlug ∷ String → EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry ∷ MonadIO μ ⇒ CategoryName → EntrySlug → μ (Maybe (EntrySlug, Value))
readEntry category slug = liftIO $ do
  doc ← readDocumentByName category slug ∷ IO (Maybe Value)
  return $ (slug, ) <$> doc

readCategory ∷ Int → Maybe Int → Maybe Int → HashMap String Value → CategoryName
             → Sweetroll ([Slice String], HashMap String Value)
readCategory perPage before after initialEntries catsName =
  foldM readCategory' ([], initialEntries) $ splitOn "+" catsName
  where readCategory' (prevSlices, prevEntries) catName = do
          ks ← listDocumentKeys catName
          let newSlice = sliceCategory perPage before after catName $ map (second $ drop 1) $ mapMaybe maybeReadIntString ks
              newSlices = case prevSlices of
                            (s : ss) → mergeSlices perPage (isJust after) newSlice s : newSlice : s : ss
                            [] → [newSlice]
          newEntries ← foldM (\entries u →
                               if u `member` entries then return entries else do
                                 entry ← fmap snd (parseEntryURIRelative $ fromJust $ parseURIReference u) >>= readEntry catName -- XXX: eliminate URI
                                 return $ case entry of
                                            Just (_, v) → insertMap u v entries
                                            Nothing → entries)
                             prevEntries
                             (map snd $ sliceItems newSlice)
          return (newSlices, newEntries)

notFoundOrGone ∷ (MonadIO μ, MonadSweetroll μ) ⇒ CategoryName → EntrySlug → μ ServantErr
notFoundOrGone catName slug = do
  deleted ← liftIO . readTVarIO =<< getDeleted
  if any (\x → catName `isPrefixOf` x && (slug ++ ".json") `isSuffixOf` x) deleted
    then renderError err410 "410"
    else renderError err404 "404"
