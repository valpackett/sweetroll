{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Data.Maybe (fromJust)
import           Data.Aeson
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
import           Sweetroll.Routes
import           Sweetroll.Pages
import           Sweetroll.Rendering
import           Sweetroll.Auth
import           Sweetroll.Micropub
import           Sweetroll.Style
import           Sweetroll.Util

getMicropub ∷ JWT VerifiedJWT → Maybe Text → Sweetroll [(Text, Text)]
getMicropub _ (Just "syndicate-to") = do
  (MkSyndicationConfig syndConf) ← getConfOpt syndicationConfig
  return $ case syndConf of
             Object o → map ("syndicate-to[]", ) $ keys o
             _ → []
getMicropub token _ = getAuth token

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return allCss

getIndex ∷ Sweetroll (WithLink (View IndexPage))
getIndex = do
  ipp ← getConfOpt itemsPerPage
  cats ← listCollections >>= mapM (readCategory ipp Nothing Nothing)
  selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy IndexRoute)
  addLinks [selfLink] $ view $ IndexPage $ map (second fromJust) $ filter visibleCat cats

getCat ∷ String → Maybe Int → Maybe Int → Sweetroll (WithLink (View CatPage))
getCat catName before after = do
  ipp ← getConfOpt itemsPerPage
  cat ← readCategory ipp before after catName
  case snd cat of
    Nothing → throwError err404
    Just slice → do
      selfLink ← genLink "self" $ sliceSelf slice
      addLinks [selfLink] $ view $ CatPage catName slice

getEntry ∷ String → String → Sweetroll (WithLink (View EntryPage))
getEntry catName slug = do
  entry ← readDocumentByName catName slug ∷ Sweetroll (Maybe Value)
  case entry of
    Nothing → throwError err404
    Just e  → do -- cacheHTTPDate (maximumMay $ entryUpdated e) $ do
      otherSlugs ← listDocumentKeys catName
      selfLink ← genLink "self" $ safeLink sweetrollAPI (Proxy ∷ Proxy EntryRoute) catName slug
      addLinks [selfLink] $ view $ EntryPage catName (map readSlug $ sort otherSlugs) (slug, e)

sweetrollServerT ∷ SweetrollCtx → ServerT SweetrollAPI Sweetroll
sweetrollServerT ctx = getIndieConfig :<|> getDefaultCss
                  :<|> postLogin :<|> AuthProtected key getAuth
                  :<|> AuthProtected key postMicropub :<|> AuthProtected key getMicropub
                  :<|> getEntry :<|> getCat :<|> getIndex
    where key = secretKey $ _ctxSecs ctx

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx = foldr ($) (sweetrollApp' ctx) [
                     staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
                   , routedMiddleware ((== (Just "bower")) . headMay) $ serveStaticFromLookup bowerComponents
                   , autohead ]
  where sweetrollApp' ∷ SweetrollCtx → Application
        sweetrollApp' = serve sweetrollAPI . sweetrollServer
        sweetrollServer ∷ SweetrollCtx → Server SweetrollAPI
        sweetrollServer c = enter (sweetrollToEither c) $ sweetrollServerT c

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = initCtx conf secs >>= return . sweetrollApp


genLink ∷ MonadSweetroll μ ⇒ Text → URI → μ L.Link
genLink rel u = do
  base ← getConfOpt baseURI
  return $ L.Link (u `relativeTo` base) [(L.Rel, rel)]

addLinks ∷ (MonadSweetroll μ, AddHeader "Link" [L.Link] α β) ⇒ [L.Link] → μ α → μ β
addLinks ls a = do
  micropub ← genLink "micropub" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostMicropubRoute)
  tokenEndpoint ← genLink "token_endpoint" $ safeLink sweetrollAPI (Proxy ∷ Proxy PostLoginRoute)
  authorizationEndpoint ← getConfOpt indieAuthRedirEndpoint >>= \x → return $ fromJust $ L.lnk x [(L.Rel, "authorization_endpoint")]
  hub ← getConfOpt pushHub >>= \x → return $ fromJust $ L.lnk x [(L.Rel, "hub")]
  return . addHeader (micropub : tokenEndpoint : authorizationEndpoint : hub : ls) =<< a



readSlug ∷ String → EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry ∷ MonadIO μ ⇒ EntrySlug → String → μ (Maybe (EntrySlug, Value))
readEntry category slug = liftIO $ do
  doc ← readDocumentByName category slug ∷ IO (Maybe Value)
  return $ (slug, ) <$> doc

readCategory ∷ MonadIO μ ⇒ Int → Maybe Int → Maybe Int → CategoryName → μ (CategoryName, Maybe (Slice (EntrySlug, Value)))
readCategory perPage before after catName = liftIO $ do
  ks ← listDocumentKeys catName
  let allItems = sortOn (negate . fst) $ map (second $ drop 1) $ mapMaybe maybeReadIntString ks
      mayFilterFst f (Just y) xs = filter ((f y) . fst) xs
      mayFilterFst _ Nothing  xs = xs
      items = take perPage $ mayFilterFst (<) after $ mayFilterFst (>) before allItems
  maybes ← mapM (readEntry catName . snd) $ items
  let slice = Slice { sliceItems  = fromMaybe [] $ sequence $ maybes
                    , sliceBefore = case (fst <$> lastMay allItems, fst <$> lastMay items) of
                                      (Just minId, Just lastId) | lastId > minId → Just $ permalink (Proxy ∷ Proxy CatRouteB) catName lastId
                                      _ → Nothing
                    , sliceSelf   = case (before, after) of
                                      (Just b, Just a)   → permalink (Proxy ∷ Proxy CatRoute)  catName b a
                                      (Just b, Nothing)  → permalink (Proxy ∷ Proxy CatRouteB) catName b
                                      (Nothing, Just a)  → permalink (Proxy ∷ Proxy CatRouteA) catName a
                                      (Nothing, Nothing) → permalink (Proxy ∷ Proxy CatRouteE) catName
                    , sliceAfter  = case (fst <$> headMay allItems, fst <$> headMay items) of
                                      (Just maxId, Just headId) | headId < maxId → Just $ permalink (Proxy ∷ Proxy CatRouteA) catName headId
                                      _ → Nothing }
  return (catName, Just slice)

visibleCat ∷ (CategoryName, Maybe (Slice (EntrySlug, Value))) → Bool
visibleCat (slug, Just cat) = (not . null $ sliceItems cat)
                              && slug /= "templates"
                              && slug /= "static"
visibleCat (_, Nothing) = False
