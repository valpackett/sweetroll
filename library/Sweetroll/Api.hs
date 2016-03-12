{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           ClassyPrelude
import           Data.Maybe (fromJust)
import           Data.Aeson
import           Data.String.Conversions
import           Data.Conduit.Shell (run, proc, conduit, ProcessException, ($|), (=$=))
import qualified Data.Conduit.Combinators as CL
import qualified Data.HashMap.Strict as HMS
import qualified Network.HTTP.Link as L
import           Network.URI
import           Network.Wai
import           Network.Wai.UrlMap
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Application.Static
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
import           Sweetroll.Style
import           Sweetroll.Proxy
import           Sweetroll.Util

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return allCss

getIndex ∷ Sweetroll (WithLink (View IndexedPage))
getIndex = do
  ipp ← getConfOpt itemsPerPage
  catNames ← getConfOpt categoriesInLanding
  (slices, entries) ← foldM (\(slices, entries) catName →
                              readCategory ipp Nothing Nothing entries catName >>= \(slices', entries') → return (slices ++ slices', entries'))
                            ([], HMS.empty)
                            catNames
  selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy IndexRoute)
  addLinks [selfLink] $ view $ IndexedPage catNames slices entries

getCat ∷ String → Maybe Int → Maybe Int → Sweetroll (WithLink (View IndexedPage))
getCat catName before after = do
  ipp ← getConfOpt itemsPerPage
  (slice : slices, entries) ← readCategory ipp before after HMS.empty catName
  guardBoolM (renderError err404 "404") (length (sliceItems slice) > 0)
  selfLink ← genLink "self" $ catLink slice
  addLinks [selfLink] $ view $ IndexedPage [catName] (slice : slices) entries

getEntry ∷ String → String → Sweetroll (WithLink (View EntryPage))
getEntry catName slug = do
  entry ← guardJustM (notFoundOrGone catName slug) $ readDocumentByName catName slug
  -- TODO: cacheHTTPDate -- don't forget responses' dates! -- 204 will be thrown before rendering!
  otherSlugs ← listDocumentKeys catName
  selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy EntryRoute) catName slug
  addLinks [selfLink] $ view $ EntryPage catName (map readSlug $ sort otherSlugs) (slug, entry)

sweetrollServerT ∷ SweetrollCtx → ServerT SweetrollAPI Sweetroll
sweetrollServerT ctx = getIndieConfig :<|> getDefaultCss
                  :<|> postLogin :<|> AuthProtected key getAuth
                  :<|> AuthProtected key postMicropub :<|> AuthProtected key getMicropub
                  :<|> receiveWebmention
                  :<|> getEntry :<|> getCat :<|> getIndex
    where key = secretKey $ _ctxSecs ctx

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx = simpleCors
                 $ autohead
                 $ mapUrls $ mount "bower" (staticApp $ embeddedSettings bowerComponents)
                         <|> mount "static" (staticApp $ defaultWebAppSettings "static")
                         <|> mount "proxy" (requestProxy ctx)
                         <|> mountRoot (serve sweetrollAPI $ sweetrollServer ctx)
  where sweetrollServer c = enter (sweetrollToEither c) $ sweetrollServerT c

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = liftM sweetrollApp $ initCtx conf secs


genLink ∷ MonadSweetroll μ ⇒ Text → URI → μ L.Link
genLink rel u = do
  base ← getConfOpt baseURI
  return $ L.Link (u `relativeTo` base) [(L.Rel, rel)]

addLinks ∷ (MonadSweetroll μ, AddHeader "Link" [L.Link] α β) ⇒ [L.Link] → μ α → μ β
addLinks ls a = do
  webmention ← genLink "webmention" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostWebmentionRoute)
  micropub ← genLink "micropub" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostMicropubRoute)
  tokenEndpoint ← genLink "token_endpoint" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostLoginRoute)
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
  foldM readCategory' ([], initialEntries) $ defaultSplitOn "+" catsName
  where readCategory' (prevSlices, prevEntries) catName = do
          ks ← listDocumentKeys catName
          let newSlice = sliceCategory perPage before after catName $ map (second $ drop 1) $ mapMaybe maybeReadIntString ks
              newSlices = case prevSlices of
                            (s : ss) → mergeSlices perPage (isJust after) newSlice s : newSlice : s : ss
                            [] → [newSlice]
          newEntries ← foldM (\entries u →
                               if u `member` entries then return entries else do
                                 entry ← liftM snd (parseEntryURIRelative $ fromJust $ parseURIReference u) >>= readEntry catName -- XXX: eliminate URI
                                 return $ case entry of
                                            Just (_, v) → insertMap u v entries
                                            Nothing → entries)
                             prevEntries
                             (map snd $ sliceItems newSlice)
          return (newSlices, newEntries)

notFoundOrGone ∷ (MonadIO μ, MonadSweetroll μ) ⇒ CategoryName → EntrySlug → μ ServantErr
notFoundOrGone catName slug = do
  isGone ← liftIO (try $ run (proc "git" [ "log", "--all", "--diff-filter=D", "--find-renames", "--name-only", "--pretty=format:" ]
                              $| conduit (CL.linesUnboundedAscii
                                          =$= CL.any (\x → cs catName `isPrefixOf` x && (cs slug ++ ".json") `isSuffixOf` x)))
                   ∷ IO (Either ProcessException Bool))
  case isGone of
    Right True → renderError err410 "410"
    _ → renderError err404 "404"
