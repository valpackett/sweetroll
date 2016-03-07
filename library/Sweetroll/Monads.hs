{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Sweetroll monad that contains the application context + some
-- functions that use it, such as HTTP client requests.
module Sweetroll.Monads where

import           ClassyPrelude
import           Control.Monad.Base
import           Control.Monad.Reader hiding (forM_)
import           Control.Monad.Except hiding (forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Unsafe
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Data.Conduit
import           Data.Maybe
import           Data.String.Conversions
import           Data.Aeson.Types
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsUtil
import           Data.IndieWeb.Authorship
import           Text.RawString.QQ
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import           Network.URI
import           Servant
import           Gitson
import           Scripting.Duktape
import           Sweetroll.Conf
import           Sweetroll.Util

data SweetrollCtx = SweetrollCtx
  { _ctxConf     ∷ SweetrollConf
  , _ctxSecs     ∷ SweetrollSecrets
  , _ctxDuk      ∷ DuktapeCtx
  , _ctxLock     ∷ MVar ()
  , _ctxHttpMgr  ∷ Manager }

type MonadSweetroll = MonadReader SweetrollCtx

instance HasHttpManager SweetrollCtx where
  getHttpManager = _ctxHttpMgr

instance HasGitsonLock Sweetroll where
  getGitsonLock = asks _ctxLock

newtype Sweetroll α = Sweetroll {
  runSweetroll ∷ ReaderT SweetrollCtx (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadError ServantErr,
            MonadSweetroll)

instance MonadBaseControl IO Sweetroll where
  type StM Sweetroll α = StM (ReaderT SweetrollCtx (ExceptT ServantErr IO)) α
  liftBaseWith f = Sweetroll $ liftBaseWith $ \x → f $ x . runSweetroll
  restoreM       = Sweetroll . restoreM

runSweetrollEither ∷ SweetrollCtx → Sweetroll α → EitherT ServantErr IO α
runSweetrollEither ctx sweet = EitherT $ liftIO $ runExceptT $ runReaderT (runSweetroll sweet) ctx

sweetrollToEither ∷ SweetrollCtx → Sweetroll :~> EitherT ServantErr IO
sweetrollToEither ctx = Nat $ runSweetrollEither ctx

initCtx ∷ SweetrollConf → SweetrollSecrets → IO SweetrollCtx
initCtx conf secs = do
  hmg ← newManager
  duk ← createDuktapeCtx
  lck ← newMVar ()
  loadTemplates $ fromJust duk
  return SweetrollCtx { _ctxConf     = conf
                      , _ctxSecs     = secs
                      , _ctxDuk      = fromJust duk
                      , _ctxLock     = lck
                      , _ctxHttpMgr  = hmg }

loadTemplates ∷ DuktapeCtx → IO ()
loadTemplates duk = do
  -- TODO: compile time check that bower deps have been fetched!
  void $ evalDuktape duk $ fromJust $ lookup "lodash/dist/lodash.min.js" bowerComponents
  void $ evalDuktape duk $ fromJust $ lookup "moment/min/moment-with-locales.min.js" bowerComponents
  void $ evalDuktape duk [r|
    var SweetrollTemplates = {}
    setTemplate = function (name, html) {
      SweetrollTemplates[name] = _.template(html, {
        'sourceURL': name, 'variable': 'scope',
        'imports': { 'templates': SweetrollTemplates }
      })
    }|]
  let setTpl tname thtml = void $ callDuktape duk Nothing "setTemplate" [ String $ cs $ dropExtensions tname, String $ cs thtml ]
  forM_ defaultTemplates $ uncurry setTpl
  hasUserTpls ← doesDirectoryExist "templates"
  when hasUserTpls $ do
    userTpls ← getDirectoryContents "templates"
    forM_ (filter (not . ("." `isPrefixOf`)) userTpls) $ \tname → do -- Avoid ., .., .DS_Store and all hidden files really
      thtml ← (try $ readFile $ "templates" </> tname) ∷ IO (Either IOException ByteString)
      case thtml of
        Right h → setTpl tname h
        Left e → void $ putStrLn $ "Error when reading template " ++ cs tname ++ ": " ++ cs (show e)

getConf ∷ MonadSweetroll μ ⇒ μ SweetrollConf
getConf = asks _ctxConf

getConfOpt ∷ MonadSweetroll μ ⇒ (SweetrollConf → Maybe α) → μ α
getConfOpt f = asks $ fromMaybe (fromJust $ f def) . f . _ctxConf

getSecs ∷ MonadSweetroll μ ⇒ μ SweetrollSecrets
getSecs = asks _ctxSecs

getRenderer ∷ MonadSweetroll μ ⇒ μ (ByteString → Value → Text)
getRenderer = liftM renderer $ asks _ctxDuk
  where renderer duk x y = txtVal $ unsafePerformIO $ callDuktape duk (Just "SweetrollTemplates") x [y]
        {-# NOINLINE renderer #-}
        txtVal (Right (Just (String t))) = t
        txtVal (Right (Just _)) = "TEMPLATE ERROR: returned something other than a string"
        txtVal (Right Nothing) = "TEMPLATE ERROR: returned nothing"
        txtVal (Left e) = "TEMPLATE ERROR: " ++ cs e

withRequest ∷ (MonadIO μ, MonadBaseControl IO μ, MonadSweetroll μ) ⇒
              Request → (Response (Source μ ByteString) → μ α) → μ α
withRequest req a =
  withResponse (req { checkStatus = \_ _ _ → Nothing }) $ \resp → do
    putStrLn $ cs $ "Request status for <" ++ show (getUri req) ++ ">: " ++ (show . statusCode . responseStatus $ resp)
    a resp

withSuccessfulRequest ∷ (MonadIO μ, MonadBaseControl IO μ, MonadSweetroll μ) ⇒
                        Request → (Response (Source μ ByteString) → μ (Maybe α)) → μ (Maybe α)
withSuccessfulRequest req a = withRequest req $ \resp → 
  if responseStatus resp `elem` [ok200, created201, accepted202, noContent204] then a resp else return Nothing

withRequestHtml ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                  URI → (Response (Source μ ByteString) → μ α) → μ α
withRequestHtml uri a = do
  req ← setUri def uri
  let req' = req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] }
  withRequest req' a

withSuccessfulRequestHtml ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                            URI → (Response (Source μ ByteString) → μ (Maybe α)) → μ (Maybe α)
withSuccessfulRequestHtml uri a = do
  req ← setUri def uri
  let req' = req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] }
  withSuccessfulRequest req' a

withFetchEntryWithAuthors ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                            URI → Response (Source μ ByteString) → (Value → (Value, [Value]) → μ α) → μ (Maybe α)
withFetchEntryWithAuthors uri resp a = do
  htmlDoc ← responseBody resp $$ sinkDoc
  let mf2Options' = mf2Options { baseUri = Just uri }
      mfRoot = parseMf2 mf2Options' $ documentRoot htmlDoc
  case headMay =<< allMicroformatsOfType "h-entry" mfRoot of
    Just mfEntry@(mfE, mfPs) → do
      authors ← entryAuthors mf2Options' (\u → withSuccessfulRequestHtml u $ \resp' → liftM Just $ responseBody resp' $$ sinkDoc) uri mfRoot mfEntry
      let addAuthors (Object o) = Object $ HMS.adjust addAuthors' "properties" o
          addAuthors x = x
          addAuthors' (Object o) = Object $ HMS.insert "author" (Array $ V.fromList $ fromMaybe [] authors) o
          addAuthors' x = x
      liftM Just $ a mfRoot (addAuthors mfE, mfPs)
    _ → return Nothing

parseEntryURI ∷ (MonadError ServantErr μ, MonadSweetroll μ) ⇒
                URI → μ (String, String)
parseEntryURI uri = do
  base ← getConfOpt baseURI
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority uri) == (uriRegName <$> uriAuthority base)
  parseEntryURIRelative uri

parseEntryURIRelative ∷ (MonadError ServantErr μ) ⇒ URI → μ (String, String)
parseEntryURIRelative uri =
  case uriPathParts uri of
    [ category, slug ] → return (category, slug)
    _ → throwError errWrongPath

errWrongDomain ∷ ServantErr
errWrongDomain = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                        , errBody    = "The target URI is not on this domain." }

errWrongPath ∷ ServantErr
errWrongPath = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                      , errBody    = "The target URI is not a resource that exists on this domain." }

errNoURIInField ∷ LByteString → ServantErr
errNoURIInField f = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                           , errBody    = "You didn't put a valid absolute URI in the '" ++ f ++ "' field of the www-form-urlencoded request body." }

guardJustP ∷ MonadError ServantErr μ ⇒ ServantErr → Maybe α → μ α
guardJustP _ (Just x) = return x
guardJustP e Nothing = throwError e

guardJustM ∷ MonadError ServantErr μ ⇒ μ ServantErr → μ (Maybe α) → μ α
guardJustM ea a = a >>= \x → case x of
                                 Just v → return v
                                 Nothing → throwError =<< ea

guardJust ∷ MonadError ServantErr μ ⇒ ServantErr → μ (Maybe α) → μ α
guardJust e = guardJustM (return e)

guardBool ∷ MonadError ServantErr μ ⇒ ServantErr → Bool → μ ()
guardBool e x = unless x $ throwError e

guardBoolM ∷ MonadError ServantErr μ ⇒ μ ServantErr → Bool → μ ()
guardBoolM ea False = throwError =<< ea
guardBoolM _ True = return ()
