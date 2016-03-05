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
import qualified Network.HTTP.Link as L
import           Network.URI
import           Network.Wai
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Routed
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
import           Sweetroll.Util

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
  slice ← guardJustM (renderError err404 "404") $ liftM snd $ readCategory ipp before after catName
  selfLink ← genLink "self" $ sliceSelf slice
  addLinks [selfLink] $ view $ CatPage catName slice

getEntry ∷ String → String → Sweetroll (WithLink (View EntryPage))
getEntry catName slug = do
  entry ← guardJustM (notFoundOrGone catName slug) $ readDocumentByName catName slug
  -- TODO: cacheHTTPDate (maximumMay $ entryUpdated e)
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
sweetrollApp ctx = foldr ($) (sweetrollApp' ctx) [
                     staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
                   , routedMiddleware ((== Just "bower") . headMay) $ serveStaticFromLookup bowerComponents
                   , autohead
                   , simpleCors ]
  where sweetrollApp' ∷ SweetrollCtx → Application
        sweetrollApp' = serve sweetrollAPI . sweetrollServer
        sweetrollServer ∷ SweetrollCtx → Server SweetrollAPI
        sweetrollServer c = enter (sweetrollToEither c) $ sweetrollServerT c

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

readCategory ∷ Int → Maybe Int → Maybe Int → CategoryName → Sweetroll (CategoryName, Maybe (Slice (EntrySlug, Value)))
readCategory perPage before after catName = do
  ks ← listDocumentKeys catName
  let slice = sliceCategory perPage before after catName $ map (second $ drop 1) $ mapMaybe maybeReadIntString ks
  maybes ← mapM (\u → liftM snd (parseEntryURIRelative u) >>= readEntry catName) $ sliceItems slice
  return $ (catName, ) $ case sequence maybes of
    Nothing → Nothing
    Just [] → Nothing
    Just ms → Just $ slice { sliceItems = ms }

visibleCat ∷ (CategoryName, Maybe (Slice (EntrySlug, Value))) → Bool
visibleCat (slug, Just cat) = (not . null $ sliceItems cat)
                              && slug /= "templates"
                              && slug /= "static"
                              && slug /= "conf"
visibleCat (_, Nothing) = False

notFoundOrGone ∷ (MonadIO μ, MonadSweetroll μ) ⇒ CategoryName → EntrySlug → μ ServantErr
notFoundOrGone catName slug = do
  isGone ← liftIO (try $ run (proc "git" [ "log", "--all", "--diff-filter=D", "--find-renames", "--name-only", "--pretty=format:" ]
                              $| conduit (CL.linesUnboundedAscii
                                          =$= CL.any (\x → cs catName `isPrefixOf` x && (cs slug ++ ".json") `isSuffixOf` x)))
                   ∷ IO (Either ProcessException Bool))
  case isGone of
    Right True → renderError err410 "410"
    _ → renderError err404 "404"
