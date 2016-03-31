{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}

module Sweetroll.Micropub.Endpoint (
  getMicropub
, postMicropub
) where

import           Sweetroll.Prelude
import           Control.Concurrent.Lifted (fork, threadDelay)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Text.Pandoc hiding (Link, Null)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Web.JWT hiding (header, decode)
import           Servant
import           System.Directory (removeFile)
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Auth
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response
import           Sweetroll.Events
import           Sweetroll.HTTPClient

getMicropub ∷ JWT VerifiedJWT → Maybe Text → [Text] → Maybe Text → Sweetroll MicropubResponse
getMicropub _ (Just "syndicate-to") _ _ = do
  (MkSyndicationConfig syndConf) ← getConfOpt syndicationConfig
  return $ SyndicateTo $ case syndConf of
             Object o → keys o
             _ → []
getMicropub _ (Just "source") props (Just url) = do
  -- TODO: props filtering
  target ← guardJustP (errNoURIInField "url") $ parseURI $ cs url
  (category, slug) ← parseEntryURI target
  entry ← guardJust errWrongPath $ readDocumentByName category slug
  return $ Source entry
getMicropub token _ _ _ = getAuth token |> AuthInfo

postMicropub ∷ JWT VerifiedJWT → MicropubRequest
             → Sweetroll (Headers '[Servant.Header "Location" Text] MicropubResponse)
postMicropub token (Create htype props synds) = do
  now ← liftIO getCurrentTime
  base ← getConfOpt baseURI
  isTest ← getConfOpt testMode
  (MkSyndicationConfig syndConf) ← getConfOpt syndicationConfig

  category ← cs <$> runCategoryDeciders (Object props)
  let slug = decideSlug props now
      absUrl = permalink (Proxy ∷ Proxy EntryRoute) category slug `relativeTo` base
  obj ← return props
        >>= fetchAllReferenceContexts
        |>  setDates now
        |>  setClientId token
        |>  setUrl absUrl
        |>  setContent (readContent =<< lookup "content" props) (getSyndLinks synds syndConf)
        |>  wrapWithType htype
  transaction "./" $ saveDocument category (formatTime defaultTimeLocale "%s-" now ++ slug) obj
  unless isTest $ void $ fork $ do
    threadDelay =<< (*1000000) `liftM` getConfOpt pushDelay
    transaction "./" $ do
      -- check that it wasn't deleted after the delay
      obj'm ← readDocumentByName category slug
      case obj'm of
        Nothing → return ()
        Just obj' → saveDocumentByName category slug =<< onPostCreated category slug absUrl obj'
  return $ addHeader (tshow absUrl) Posted

postMicropub _ (Update url upds) = do
  base ← getConfOpt baseURI
  isTest ← getConfOpt testMode
  (category, slug) ← parseEntryURI =<< guardJustP errWrongPath (parseURI $ cs url)
  let absUrl = permalink (Proxy ∷ Proxy EntryRoute) category slug `relativeTo` base -- Rebuild URL just in case
  obj ← guardJust errWrongPath $ readDocumentByName category slug
  let newObj = obj & key "properties" %~ (\o → foldl' applyUpdates o upds)
  transaction "./" $ do
    saveDocumentByName category slug newObj
    unless isTest $ void $ fork $
      saveDocumentByName category slug =<< onPostUpdated category slug absUrl obj newObj
  throwError respNoContent

postMicropub _ (Delete url) = do
  base ← getConfOpt baseURI
  isTest ← getConfOpt testMode
  deleted ← getDeleted
  (category, slug) ← parseEntryURI =<< guardJustP errWrongPath (parseURI $ cs url)
  let absUrl = permalink (Proxy ∷ Proxy EntryRoute) category slug `relativeTo` base
  transaction "./" $ do
    k ← guardJust errWrongPath $ documentFullKey category $ findByName slug
    mobj ← readDocumentByName category slug
    let filePath = category </> k <.> "json"
    liftIO $ removeFile filePath
    atomically $ modifyTVar' deleted (cons filePath)
    unless isTest $ void $ fork $ onPostDeleted category slug absUrl mobj
  throwError respNoContent


respNoContent ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respNoContent = ServantErr { errHTTPCode = 204
                           , errReasonPhrase = "No Content"
                           , errHeaders = [ ]
                           , errBody    = "" }

getSyndLinks ∷ [ObjSyndication] → Value → LText
getSyndLinks synds syndConf =
  case syndConf of
    Object o → cs $ concat $ mapMaybe (^? _String) $ mapMaybe (flip lookup o) $ filter inSyndicateTo $ keys o
    _ → ""
  where inSyndicateTo x = any (x `isInfixOf`) synds

setDates ∷ UTCTime → ObjProperties → ObjProperties
setDates now = insertMap "updated" (toJSON [ now ]) . insertWith (\_ x → x) "published" (toJSON [ now ])

setClientId ∷ JWT VerifiedJWT → ObjProperties → ObjProperties
setClientId token = insertMap "client-id" $ toJSON $ filter (/= "example.com") $ catMaybes [ lookup "client_id" $ unregisteredClaims $ claims token ]

setUrl ∷ URI → ObjProperties → ObjProperties
setUrl url = insertMap "url" $ toJSON  [ tshow url ]

setContent ∷ Maybe Pandoc → LText → ObjProperties → ObjProperties
setContent content syndLinks x = if isNothing $ (Object x) ^? key "content" . nth 0 . key "html" . _String
                                    then insertMap "content" (toJSON [ object [ "html" .= h ] ]) x
                                    else x
  where h = cs syndLinks ++ (fromMaybe "" $ (renderHtml . writeHtml pandocWriterOptions) `liftM` content)

wrapWithType ∷ ObjType → ObjProperties → Value
wrapWithType htype props =
  object [ "type"       .= [ htype ]
         , "properties" .= insertMap "syndication" (Array V.empty) props ]

decideSlug ∷ ObjProperties → UTCTime → EntrySlug
decideSlug props now = unpack . fromMaybe fallback $ getProp "slug"
  where fallback = slugify . fromMaybe (formatTimeSlug now) $ getProp "name" <|> getProp "summary"
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"
        getProp k = firstStr (Object props) (key k)

readContent ∷ Value → Maybe Pandoc
readContent c = asum [ readWith readTextile    $ key "textile"
                     , readWith readOrg        $ key "org"
                     , readWith readRST        $ key "rst"
                     , readWith readLaTeX      $ key "tex"
                     , readWith readLaTeX      $ key "latex"
                     , readWith readMarkdown   $ key "markdown"
                     , readWith readMarkdown   $ key "gfm"
                     , readWith readCommonMark $ key "value"
                     , readWith readCommonMark id ]
  where readWith rdr l = pandocRead rdr . cs <$> firstStr c l

applyUpdates ∷ Value → MicropubUpdate → Value
applyUpdates (Object props) (ReplaceProps newProps) =
  Object $ foldl' (\ps (k, v) → HMS.insert k v ps) props (HMS.toList newProps)
applyUpdates (Object props) (AddToProps newProps) =
  Object $ foldl' (\ps (k, v) → HMS.insertWith add k v ps) props (HMS.toList newProps)
    where add (Array new) (Array old) = Array $ new ++ old
          add new (Array old) = Array $ cons new old
          add _ old = old
applyUpdates (Object props) (DelFromProps newProps) =
  Object $ foldl' (\ps (k, v) → HMS.insertWith del k v ps) props (HMS.toList newProps)
    where del (Array new) (Array old) = Array $ filter (not . (flip elem new)) old
          del new (Array old) = Array $ filter (/= new) old
          del _ old = old
applyUpdates (Object props) (DelProps newProps) =
  Object $ foldl' (\ps k → HMS.delete k ps) props newProps
applyUpdates x _ = x
