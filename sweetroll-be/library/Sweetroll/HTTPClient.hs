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
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Data.HashMap.Strict (adjust)
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsUtil
import           Data.IndieWeb.SiloToMicroformats
import           Data.IndieWeb.Authorship
import           Network.HTTP.Types
import           Sweetroll.Conf (mf2Options)

performWithHtml ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ Request → EitherT Text μ (Response XDocument)
performWithHtml = performWithFn ($$ sinkDoc) . (\req → req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] })

fetchEntryWithAuthors ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ URI → Response XDocument → μ (Maybe Value, Value)
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

fetchReferenceContexts ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ Text → Object → μ Object
fetchReferenceContexts k props = do
    newCtxs ← updateCtxs $ lookup k props
    return $ insertMap k newCtxs props
  where updateCtxs (Just (Array v)) = (Array . reverse) `liftM` mapM fetch v
        updateCtxs _ = return $ Array V.empty
        fetch v@(Object _) = maybe (return v) (fetch . String) (v ^? key "properties" . key "url" . nth 0 . _String)
        fetch (String u) = maybeT (return $ String u) return $ do
          uri ← hoistMaybe $ parseURI $ cs u
          resp ← hoistMaybe =<< (liftM hush $ runHTTP $ reqU uri >>= performWithHtml)
          ewa ← fetchEntryWithAuthors uri resp
          case ewa of
            (Just (Object entry), _) → do
              prs ← lift $ fetchAllReferenceContexts $ fromMaybe (HMS.fromList []) $ Object entry ^? key "properties" . _Object
              return $ Object $ insertMap "properties" (Object prs) $ insertMap "fetched-url" (toJSON u) entry
            _ → mzero
        fetch x = return x

fetchAllReferenceContexts ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ Object → μ Object
fetchAllReferenceContexts = fetchReferenceContexts "in-reply-to"
                        >=> fetchReferenceContexts "like-of"
                        >=> fetchReferenceContexts "repost-of"
                        >=> fetchReferenceContexts "quotation-of"
