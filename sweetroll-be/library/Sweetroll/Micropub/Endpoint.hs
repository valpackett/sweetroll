{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, TypeOperators, DataKinds #-}

module Sweetroll.Micropub.Endpoint (
  getMicropub
, postMicropub
, postMedia
) where

import           Sweetroll.Prelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as MS
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteArray.Encoding
import           Data.Maybe (fromJust)
import qualified CMark as CM
import qualified CMark.Highlight as CM
import           Web.JWT hiding (header, decode)
import           Servant
import           System.Process (readProcessWithExitCode)
import           System.Directory (removeFile, renameFile)
import           System.FilePath.Posix (takeExtension)
import           Network.Wai.Parse
import           Network.Mime
import           Servant.Server.Internal
import           Control.Monad.Trans.Resource (runResourceT, withInternalState)
import           Crypto.Hash
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response
import           Sweetroll.Events
import           Sweetroll.HTTPClient
import           Sweetroll.Database

getMicropub ∷ JWT VerifiedJWT → Maybe Text → [Text] → Maybe Text → Sweetroll MicropubResponse
getMicropub _ (Just "source") props (Just url) = do
  -- TODO: props filtering
  -- TODO: check domain against host header (in sql)
  result ← queryDb url getObject
  case result of
    Right (Just obj) → return $ Source obj
    Right Nothing → throwErrText err404 "Entry not found."
    Left x  → throwErrText err500 $ "Database error: " ++ cs (show x)
getMicropub _ (Just "syndicate-to") _ _ = do
  let (MkSyndicationConfig syndConf) = syndicationConfig
  return $ SyndicateTo $ case syndConf of
             Object o → map (\(k, v) → object [ "uid" .= v, "name" .= k ]) $ HMS.toList o
             Array a → toList a
             _ → []
getMicropub _ (Just "media-endpoint") _ _ = do
  base ← getBaseURI
  url ← fromMaybe nullURI . parseURIReference <$> getConfOpt mediaEndpoint
  return $ MediaEndpoint $ tshow $ url `relativeTo` base
getMicropub token (Just "config") props url =
  liftM MultiResponse $ mapM (\x → getMicropub token (Just x) props url)
                             [ "media-endpoint", "syndicate-to" ]
--getMicropub token _ _ _ = getAuth token |> AuthInfo
getMicropub token _ props url = getMicropub token (Just "media-endpoint") props url


extMap ∷ Map ByteString Text
extMap = MS.foldlWithKey' (\a x y → MS.insert y x a) MS.empty defaultMimeMap

postMedia ∷ JWT VerifiedJWT → MultiPartDataT Tmp → Sweetroll (Headers '[Servant.Header "Location" Text] MicropubResponse)
postMedia _ multipart = do
  nameRef ← newIORef ""
  liftIO $ void $ multipart $ \(params, files) → do
    forM_ files $ \(name, file) → when (name == "file") $ do
      content ← BL.readFile $ fileContent file
      let digest = cs $ asByteString $ convertToBase Base16 (hashlazy content ∷ Digest SHA256)
      let fileExt = takeExtension $ cs $ fileName file
      let ext = if fileContentType file == "application/octet-stream"
                   then fileExt
                   else fromMaybe fileExt $ (("." ++) . cs) <$> lookup (fileContentType file) extMap
      let fullname = "static/" ++ digest ++ ext
      renameFile (fileContent file) fullname
      writeIORef nameRef fullname
    return (params, files)
  base ← getBaseURI
  name ← readIORef nameRef
  let uri = tshow $ (fromMaybe nullURI $ parseURIReference name) `relativeTo` base
  return $ addHeader uri Posted


postMicropub ∷ JWT VerifiedJWT → MicropubRequest
             → Sweetroll (Headers '[Servant.Header "Location" Text] MicropubResponse)
postMicropub token (Create htype props synds) = do
  now ← liftIO getCurrentTime
  base ← getBaseURI
  isTest ← getConfOpt testMode

  category ← cs <$> runCategoryDeciders (Object props)
  let slug = decideSlug props now
      absUrl = (fromJust $ parseURI $ "/" ++ category ++ "/" ++ slug) `relativeTo` base -- XXX
  obj ← return props
        >>= fetchAllReferenceContexts
        |>  setDates now
        |>  setClientId token
        |>  setUrl absUrl
        |>  setContent (readContent =<< lookup "content" props) (intercalate " " synds)
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
  base ← getBaseURI
  isTest ← getConfOpt testMode
  (category, slug) ← parseEntryURI =<< guardJustP errWrongPath (parseURI $ cs url)
  -- Rebuild URL just in case
  let absUrl = (fromJust $ parseURI $ "/" ++ category ++ "/" ++ slug) `relativeTo` base -- XXX
  obj ← guardJust errWrongPath $ readDocumentByName category slug
  let newObj = obj & key "properties" %~ (\o → foldl' applyUpdates o upds)
  transaction "./" $ do
    saveDocumentByName category slug newObj
    unless isTest $ void $ fork $
      saveDocumentByName category slug =<< onPostUpdated category slug absUrl obj newObj
  throwError respNoContent

postMicropub _ (Delete url) = do
  base ← getBaseURI
  isTest ← getConfOpt testMode
  deleted ← getDeleted
  (category, slug) ← parseEntryURI =<< guardJustP errWrongPath (parseURI $ cs url)
  let absUrl = (fromJust $ parseURI $ "/" ++ category ++ "/" ++ slug) `relativeTo` base -- XXX
  transaction "./" $ do
    k ← guardJust errWrongPath $ documentFullKey category $ findByName slug
    mobj ← readDocumentByName category slug
    let filePath = category </> k <.> "json"
    liftIO $ removeFile filePath
    atomically $ modifyTVar' deleted (cons filePath)
    unless isTest $ void $ fork $ onPostDeleted category slug absUrl mobj
  throwError respNoContent

postMicropub _ (Undelete url) = do
  base ← getBaseURI
  isTest ← getConfOpt testMode
  deleted ← getDeleted
  (category, slug) ← parseEntryURI =<< guardJustP errWrongPath (parseURI $ cs url)
  filePath ← guardJust errWrongPath $ (find (\e → category `isPrefixOf` e && slug `isInfixOf` e)) `liftM` atomically (readTVar deleted)
  let absUrl = (fromJust $ parseURI $ "/" ++ category ++ "/" ++ slug) `relativeTo` base -- XXX
  transaction "./" $ do
    _ ← liftIO $ readProcessWithExitCode "git" [ "checkout", filePath ] ""
    atomically $ modifyTVar' deleted (filter (/= filePath))
    mobj ← readDocumentByName category slug
    case mobj of
      Just obj → unless isTest $ void $ fork $
        saveDocumentByName category slug =<< onPostUndeleted category slug absUrl obj
      Nothing → return ()
  throwError respNoContent


respNoContent ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respNoContent = ServantErr { errHTTPCode = 204
                           , errReasonPhrase = "No Content"
                           , errHeaders = [ ]
                           , errBody    = "" }

setDates ∷ UTCTime → ObjProperties → ObjProperties
setDates now = insertMap "updated" (toJSON [ now ]) . insertWith (\_ x → x) "published" (toJSON [ now ])

setClientId ∷ JWT VerifiedJWT → ObjProperties → ObjProperties
setClientId token = insertMap "client-id" $ toJSON $ filter (/= "example.com") $ catMaybes [ lookup "client_id" $ unregisteredClaims $ claims token ]

setUrl ∷ URI → ObjProperties → ObjProperties
setUrl url = insertMap "url" $ toJSON  [ tshow url ]

setContent ∷ Maybe CM.Node → Text → ObjProperties → ObjProperties
setContent content syndLinks x =
  if isNothing $ (Object x) ^? key "content" . nth 0 . key "html" . _String
     then insertMap "content" (toJSON [ object [ "html" .= h ] ]) x
     else x -- XXX: add syndLinks here!!!
  where h = cs syndLinks ++ (fromMaybe "" $ rndr `liftM` content)
        rndr = CM.nodeToHtml cmarkOptions . CM.highlightNode

wrapWithType ∷ ObjType → ObjProperties → Value
wrapWithType htype props =
  object [ "type"       .= [ htype ]
         , "properties" .= insertMap "syndication" (Array V.empty) props ]

decideSlug ∷ ObjProperties → UTCTime → EntrySlug
decideSlug props now = unpack . fromMaybe fallback $ getProp "mp-slug"
  where fallback = slugify . fromMaybe (formatTimeSlug now) $ getProp "name"
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"
        getProp k = firstStr (Object props) (key k)

readContent ∷ Value → Maybe CM.Node
readContent c = CM.commonmarkToNode cmarkOptions <$>
                  (    firstStr c (key "markdown")
                   <|> firstStr c (key "value")
                   <|> firstStr c id)

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


-- XXX: From https://github.com/haskell-servant/servant/issues/133 -- delete when support lands in servant

class KnownBackend b where
  type Storage b ∷ *
  withBackend ∷ Proxy b → (BackEnd (Storage b) → IO r) → IO r

instance KnownBackend Tmp where
  type Storage Tmp = FilePath
  withBackend Proxy f = runResourceT . withInternalState $ \s →
    f (tempFileBackEnd s)


type MultiPartData b = ([Param], [File (Storage b)])
type MultiPartDataT b = ((MultiPartData b → IO (MultiPartData b)) → IO (MultiPartData b))

instance (KnownBackend b, HasServer sublayout context) => HasServer (Files b :> sublayout) context where
  type ServerT (Files b :> sublayout) m =
    MultiPartDataT b → ServerT sublayout m

  route Proxy context subserver =
    route (Proxy ∷ Proxy sublayout) context (addBodyCheck subserver bodyCheck)
    where
      bodyCheck = withRequest $ \request → return (\f →
        withBackend (Proxy ∷ Proxy b) $ \pb → parseRequestBody pb request >>= f)
