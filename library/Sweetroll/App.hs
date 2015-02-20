{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

-- | The module that contains the Sweetroll WAI application.
module Sweetroll.App (app) where

import           ClassyPrelude
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           Network.HTTP.Types.Status
import           Network.HTTP.Link
import           Text.Pandoc hiding (Link)
import           Text.Highlighting.Kate.Format.HTML (styleToCss)
import           Text.Read (readMaybe)
import           Web.Scotty.Trans
import           Gitson
import           Gitson.Util (maybeReadIntString)
import           Data.Maybe (fromJust)
import           Data.Stringable
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Conf
import           Sweetroll.Auth
import           Sweetroll.Pages hiding (render)
import qualified Sweetroll.Pages as P
import           Sweetroll.Pagination
import           Sweetroll.Micropub
import           Sweetroll.Syndication (showSyndication)

app ∷ SweetrollApp
app = do

  conf ← getConf
  hostInfo ← getHostInfo

  let base = baseUrl conf
      checkAuth' = if testMode conf then id else checkAuth unauthorized
      links = [ Link (mkUrl base ["micropub"])                [(Rel, "micropub")]
              , Link (mkUrl base ["login"])                   [(Rel, "token_endpoint")]
              , Link (pack $ indieAuthRedirEndpoint conf)     [(Rel, "authorization_endpoint")] ]
      addLinks l x = addHeader "Link" (fromStrict $ writeLinkHeader l) >> x
      authorHtml = renderRaw (authorTemplate conf) hostInfo
      render = P.render conf authorHtml
      pageNotFound = status notFound404 >> render notFoundTemplate notFoundView

  middleware autohead -- XXX: does not add Content-Length
  middleware $ staticPolicy $ noDots >-> isNotAbsolute >-> addBase "static"
  middleware logStdoutDev

  get "/default-style.css" $ setHeader "Content-Type" "text/css" >> raw (defaultStyle conf ++
    (toLazyByteString $ styleToCss $ writerHighlightStyle pandocWriterOptions))

  post "/login" $ doIndieAuth unauthorized

  get "/micropub" $ checkAuth' $ showSyndication $ showAuth

  post "/micropub" $ checkAuth' $ doMicropub

  get "/" $ addLinks links $ do
    cats ← listCollections >>= mapM (readCategory (itemsPerPage conf) (-1))
    render indexTemplate $ indexView conf $ map (\(x, y) → (x, fromJust y)) $ filter visibleCat cats

  get "/:category" $ addLinks links $ do
    catName ← param "category"
    allParams ← params
    let pageNumber = fromMaybe (-1) (readMaybe . toString =<< findByKey allParams "page")
    cat ← readCategory (itemsPerPage conf) pageNumber catName
    case snd cat of
      Nothing → pageNotFound
      Just p → render categoryTemplate $ catView conf catName p

  get "/:category/:slug" $ addLinks links $ do
    category ← param "category"
    slug ← param "slug"
    entry ← readDocumentByName category slug ∷ SweetrollAction (Maybe Entry)
    case entry of
      Nothing → pageNotFound
      Just e  → do
        otherSlugs ← listDocumentKeys category
        render entryTemplate $ entryView category (map readSlug otherSlugs) (slug, e)

  notFound pageNotFound

visibleCat ∷ (CategoryName, Maybe (Page (EntrySlug, Entry))) → Bool
visibleCat (slug, Just cat) = (not $ null $ items cat)
                              && slug /= "templates"
                              && slug /= "static"
visibleCat (_, Nothing) = False

readSlug ∷ String → EntrySlug
readSlug x = drop 1 $ fromMaybe "-404" $ snd <$> maybeReadIntString x -- errors should never happen

readEntry ∷ MonadIO i ⇒ CategoryName → String → i (Maybe (EntrySlug, Entry))
readEntry category fname = liftIO $ do
  doc ← readDocument category fname ∷ IO (Maybe Entry)
  return $ (\x → (readSlug fname, x)) <$> doc

readCategory ∷ MonadIO i ⇒ Int → Int → CategoryName → i (CategoryName, Maybe (Page (EntrySlug, Entry)))
readCategory perPage pageNumber c = liftIO $ do
  slugs ← listDocumentKeys c
  case paginate True perPage pageNumber slugs of
    Nothing → return (c, Nothing)
    Just page → do
      maybes ← mapM (readEntry c) $ items page
      return (c, Just $ changeItems page $ fromMaybe [] $ sequence maybes)
