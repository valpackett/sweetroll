{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, FlexibleInstances, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
-- import           Data.Stringable
import           Data.Maybe (fromJust)
import           Data.Microformats2
import           Data.Microformats2.Aeson ()
import           Network.Wai
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           Servant
-- import           Servant.Server.Internal
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

type SweetrollAPI = "login" :> QueryParam "me" Text :> QueryParam "code" Text :> QueryParam "redirect_uri" Text :> QueryParam "client_id" Text :> QueryParam "state" Text :> Post '[FormUrlEncoded] [(Text, Text)]
               :<|> "micropub" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> AuthProtected :> Post '[FormUrlEncoded] (Headers '[Header "Location" Text] [(Text, Text)])
               :<|> "micropub" :> QueryParam "q" Text :> AuthProtected :> Get '[FormUrlEncoded] [(Text, Text)]
               :<|> "indie-config" :> Get '[HTML] IndieConfig
               :<|> "default-style.css" :> Get '[CSS] LByteString
               :<|> Capture "catName" String :> QueryParam "page" Int :> Get '[HTML] Text
               :<|> Capture "catName" String :> Capture "slug" String :> Get '[HTML] Text
               :<|> Get '[HTML] Text

getMicropub ∷ Maybe Text → JWT VerifiedJWT → Sweetroll [(Text, Text)]
getMicropub (Just "syndicate-to") _ = getSyndication
getMicropub _ token = getAuth token

getIndieConfig ∷ Sweetroll IndieConfig
getIndieConfig = getConfOpt indieConfig

getDefaultCss ∷ Sweetroll LByteString
getDefaultCss = return allCss

getIndex ∷ Sweetroll Text
getIndex = do
  -- addLinks $ (Link (mkUrl base []) [(Rel, "self")]) : links
  ipp ← getConfOpt itemsPerPage
  cats ← listCollections >>= mapM (readCategory ipp (-1))
  renderIndex $ map (second fromJust) $ filter visibleCat cats

getCat ∷ String → Maybe Int → Sweetroll Text
getCat catName page = do
  -- addLinks $ (Link (mkUrl base [pack catName]) [(Rel, "self")]) : links
  ipp ← getConfOpt itemsPerPage
  cat ← readCategory ipp (fromMaybe (-1) page) catName
  case snd cat of
    Nothing → throwError err404
    Just p → renderCat catName p

getEntry ∷ String → String → Sweetroll Text
getEntry catName slug = do
  -- addLinks links
  entry ← readDocumentByName catName slug ∷ Sweetroll (Maybe Entry)
  case entry of
    Nothing → throwError err404
    Just e  → do -- cacheHTTPDate (maximumMay $ entryUpdated e) $ do
      otherSlugs ← listDocumentKeys catName
      renderEntry catName (map readSlug $ sort otherSlugs) (slug, e)

sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT =
       postLogin :<|> postMicropub :<|> getMicropub :<|> getIndieConfig :<|> getDefaultCss
  :<|> getCat :<|> getEntry :<|> getIndex

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx = foldr ($) (sweetrollApp' ctx) [
                     logStdoutDev
                   , staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
                   , autohead]
  where sweetrollApp' ∷ SweetrollCtx → Application
        sweetrollApp' = serve sweetrollAPI . sweetrollServer
        sweetrollServer ∷ SweetrollCtx → Server SweetrollAPI
        sweetrollServer c = enter (sweetrollToEither c) sweetrollServerT
        sweetrollAPI ∷ Proxy SweetrollAPI
        sweetrollAPI = Proxy

initSweetrollApp ∷ SweetrollConf → SweetrollTemplates → SweetrollSecrets → IO Application
initSweetrollApp conf tpls secs = initCtx conf tpls secs >>= return . sweetrollApp


readSlug ∷ String → EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry ∷ MonadIO i ⇒ CategoryName → String → i (Maybe (EntrySlug, Entry))
readEntry category fname = liftIO $ do
  doc ← readDocument category fname ∷ IO (Maybe Entry)
  return $ (\x → (readSlug fname, x)) <$> doc

readCategory ∷ MonadIO i ⇒ Int → Int → CategoryName → i (CategoryName, Maybe (Page (EntrySlug, Entry)))
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
