{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, ConstraintKinds #-}

-- | All the things related to making HTTP requests and parsing them.
module Sweetroll.HTTPClient (
  module Sweetroll.HTTPClient
, module Network.HTTP.Types
, Response
, responseStatus
, responseHeaders
, responseBody
) where

import           Sweetroll.Prelude
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Data.HashMap.Strict (adjust)
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsUtil
import           Data.IndieWeb.SiloToMicroformats
import           Data.IndieWeb.Authorship
import           Network.HTTP.Types
import           Network.HTTP.Conduit as HC
import           Network.HTTP.Client.Conduit as HCC
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Sweetroll.Conf (mf2Options)

type MonadHTTP ψ μ = (HasHttpManager ψ, MonadReader ψ μ, MonadIO μ, MonadBaseControl IO μ)

runHTTP = runEitherT

reqU ∷ (MonadHTTP ψ μ) ⇒ URI → EitherT Text μ Request
reqU uri = hoistEither $ bimap tshow id $ setUri def uri

reqS ∷ (MonadHTTP ψ μ, ConvertibleStrings σ String) ⇒ σ → EitherT Text μ Request
reqS uri = hoistEither $ bimap tshow id $ parseUrl $ cs uri

anyStatus ∷ (MonadHTTP ψ μ) ⇒ Request → EitherT Text μ Request
anyStatus req = return req { checkStatus = \_ _ _ → Nothing }

postForm ∷ (MonadHTTP ψ μ) ⇒ [(Text, Text)] → Request → EitherT Text μ Request
postForm form req =
  return req { method = "POST"
             , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
             , requestBody = RequestBodyBS $ writeForm form }

performWithFn ∷ (MonadHTTP ψ μ) ⇒ (ConduitM ι ByteString μ () → μ ρ) → Request → EitherT Text μ (Response ρ)
performWithFn fn req = do
  res ← lift $ tryAny $ HCC.withResponse req $ \res → do
    putStrLn $ cs $ "Request status for <" ++ show (getUri req) ++ ">: " ++ (show . statusCode . responseStatus $ res)
    body ← fn $ responseBody res
    return res { responseBody = body }
  hoistEither $ bimap tshow id res

performWithVoid ∷ (MonadHTTP ψ μ) ⇒ Request → EitherT Text μ (Response ())
performWithVoid = performWithFn (const $ return ())

performWithBytes ∷ (MonadHTTP ψ μ) ⇒ Request → EitherT Text μ (Response LByteString)
performWithBytes = performWithFn ($$ C.sinkLazy)

performWithHtml ∷ (MonadHTTP ψ μ, MonadThrow μ) ⇒ Request → EitherT Text μ (Response XDocument)
performWithHtml = performWithFn ($$ sinkDoc) . (\req → req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] })

fetchEntryWithAuthors ∷ (MonadHTTP ψ μ, MonadThrow μ) ⇒ URI → Response XDocument → μ (Maybe Value, Value)
fetchEntryWithAuthors uri res = do
  let mf2Options' = mf2Options { baseUri = Just uri }
      mfRoot = parseMf2 mf2Options' $ documentRoot $ responseBody res
  he ← case headMay =<< allMicroformatsOfType "h-entry" mfRoot of
    Just mfEntry@(mfE, _) → do
      let fetch uri' = fmap (\x → responseBody <$> hush x) $ runHTTP $ reqU uri' >>= performWithHtml
      authors ← entryAuthors mf2Options' fetch uri mfRoot mfEntry
      let addAuthors (Object o) = Object $ adjust addAuthors' "properties" o
          addAuthors x = x
          addAuthors' (Object o) = Object $ insertMap "author" (Array $ fromList $ fromMaybe [] authors) o
          addAuthors' x = x
      return $ Just $ addAuthors mfE
    _ → return $ parseTwitter mf2Options' $ documentRoot $ responseBody res
  return $ case he of
             Just mfE → (Just mfE, mfRoot)
             _ → (Nothing, mfRoot)

fetchReplyContexts ∷ (MonadHTTP ψ μ, MonadThrow μ) ⇒ Text → Object → μ Object
fetchReplyContexts k props = do
    newCtxs ← updateCtxs $ lookup k props
    return $ insertMap k newCtxs props
  where updateCtxs (Just (Array v)) = Array `liftM` mapM fetch v
        updateCtxs _ = return $ Array V.empty
        fetch v@(Object _) = maybe (return v) (fetch . String) (v ^? key "properties" . key "url" . nth 0 . _String)
        fetch (String u) = maybeT (return $ String u) return $ do
          uri ← hoistMaybe $ parseURI $ cs u
          resp ← hoistMaybe =<< (liftM hush $ runHTTP $ reqU uri >>= performWithHtml)
          ewa ← fetchEntryWithAuthors uri resp
          case ewa of
            (Just (Object entry), _) → do
              prs ← lift $ fetchAllReplyContexts $ fromMaybe (HMS.fromList []) $ (Object entry) ^? key "properties" . _Object
              return $ Object $ insertMap "properties" (Object prs) $ insertMap "fetched-url" (toJSON u) entry
            _ → mzero
        fetch x = return x

fetchAllReplyContexts ∷ (MonadHTTP ψ μ, MonadThrow μ) ⇒ Object → μ Object
fetchAllReplyContexts = fetchReplyContexts "in-reply-to"
                    >=> fetchReplyContexts "like-of"
                    >=> fetchReplyContexts "repost-of"
                    >=> fetchReplyContexts "quotation-of"
