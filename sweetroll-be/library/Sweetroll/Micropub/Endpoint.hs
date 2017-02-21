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
import           System.Directory (renameFile)
import           System.FilePath.Posix (takeExtension)
import           Network.Wai.Parse
import           Network.Mime
import           Servant.Server.Internal
import           Control.Monad.Trans.Resource (runResourceT, withInternalState)
import           Crypto.Hash
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response
import           Sweetroll.HTTPClient
import           Sweetroll.Database


getMicropub ∷ JWT VerifiedJWT → Maybe Text → Maybe Text → [Text] → Maybe Text → Sweetroll MicropubResponse
getMicropub _ host (Just "source") props (Just url) = do
  -- TODO: props filtering
  ensureRightDomain (base host) $ parseUri url
  obj ← guardEntryNotFound =<< guardDbError =<< queryDb url getObject
  return $ Source obj
getMicropub _ _ (Just "syndicate-to") _ _ = do
  -- TODO: put that into the domain's config
  let (MkSyndicationConfig syndConf) = syndicationConfig
  return $ SyndicateTo $ case syndConf of
             Object o → map (\(k, v) → object [ "uid" .= v, "name" .= k ]) $ HMS.toList o
             Array a → toList a
             _ → []
getMicropub _ host (Just "media-endpoint") _ _ = do
  url ← fromMaybe nullURI . parseURIReference <$> getConfOpt mediaEndpoint
  return $ MediaEndpoint $ tshow $ url `relativeTo` base host
getMicropub token host (Just "config") props url =
  liftM MultiResponse $ mapM (\x → getMicropub token host (Just x) props url)
                             [ "media-endpoint", "syndicate-to" ]
--getMicropub token _ _ _ _ = getAuth token |> AuthInfo
getMicropub token host _ props url = getMicropub token host (Just "media-endpoint") props url


extMap ∷ Map ByteString Text
extMap = MS.foldlWithKey' (\a x y → MS.insert y x a) MS.empty defaultMimeMap

postMedia ∷ JWT VerifiedJWT → Maybe Text → MultiPartDataT Tmp → Sweetroll (Headers '[Servant.Header "Location" Text] MicropubResponse)
postMedia _ host multipart = do
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
  name ← readIORef nameRef
  let uri = tshow $ (fromMaybe nullURI $ parseURIReference name) `relativeTo` base host
  return $ addHeader uri Posted


postMicropub ∷ JWT VerifiedJWT → Maybe Text → MicropubRequest
             → Sweetroll (Headers '[Servant.Header "Location" Text] MicropubResponse)
postMicropub token host (Create htype props synds) = do
  now ← liftIO getCurrentTime
  category ← cs <$> runCategoryDeciders (Object props)
  let slug = decideSlug props now
      absUrl = (fromJust $ parseURIReference $ "/" ++ category ++ "/" ++ slug) `relativeTo` base host
  obj ← return props
        >>= fetchAllReferenceContexts
        |>  setDates now
        |>  setClientId token
        |>  setUrl absUrl -- TODO: allow any on the correct domain
        |>  setContent (readContent =<< lookup "content" props) (intercalate " " synds)
        |>  wrapWithType htype
  guardDbError =<< queryDb obj upsertObject
  return $ addHeader (tshow absUrl) Posted

postMicropub _ host (Update url upds) = do
  ensureRightDomain (base host) $ parseUri url
  obj ← guardEntryNotFound =<< guardDbError =<< queryDb url getObject
  let newObj = obj & key "properties" %~ (\o → foldl' applyUpdates o upds)
  -- TODO ensure domain not modified
  guardDbError =<< queryDb newObj upsertObject
  throwError respNoContent

postMicropub _ host (Delete url) = do
  ensureRightDomain (base host) $ parseUri url
  guardDbError =<< queryDb url deleteObject
  throwError respNoContent

postMicropub _ host (Undelete url) = do
  ensureRightDomain (base host) $ parseUri url
  guardDbError =<< queryDb url undeleteObject
  throwError respNoContent


respNoContent ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respNoContent = ServantErr { errHTTPCode = 204
                           , errReasonPhrase = "No Content"
                           , errHeaders = [ ]
                           , errBody    = "" }

runCategoryDeciders ∷ MonadIO μ ⇒ Value → μ Text
runCategoryDeciders v = do
  return "notes" -- XXX

setDates ∷ UTCTime → ObjProperties → ObjProperties
setDates now = insertMap "updated" (toJSON [ now ]) . insertWith (\_ x → x) "published" (toJSON [ now ])

setClientId ∷ JWT VerifiedJWT → ObjProperties → ObjProperties
setClientId token = insertMap "client-id" $ toJSON $ filter (/= "example.com") $ catMaybes [ lookup "client_id" $ unregisteredClaims $ claims token ]

setUrl ∷ URI → ObjProperties → ObjProperties
setUrl url = insertMap "url" $ toJSON [ tshow url ]

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
