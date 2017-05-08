{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, ConstraintKinds, LambdaCase, TemplateHaskell #-}

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
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
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

fetchLinkedEntires' ∷ (MonadHTTP ψ μ, MonadCatch μ, MonadLogger μ) ⇒ Int → Set URI → Set URI → Object → μ Object
fetchLinkedEntires' 0 _ _ props = return props
fetchLinkedEntires' depthLeft excludedDomains excludedURLs props = do
  (flip HMS.traverseWithKey) props $ curry $ \case
    (k, (Array v)) | k `notElem` excludedKeys → Array <$> forM v fetchIfAllowed
    (_, v) → return v
  where fetchIfAllowed v@(Object o) = maybe (do 
              $logInfo$ "Could not extract URL from object for fetching: " ++ tshow o
              return v)
            (fetchIfAllowed . String)
            (v ^? key "properties" . key "url" . nth 0 . _String)
        fetchIfAllowed (String u) | isNothing (parseURI $ cs u) = do
          $logInfo$ "Not a parsable URL for fetching: '" ++ u ++ "'"
          return (String u)
        fetchIfAllowed (String u)
          | parseUri u `notElem` excludedURLs
            && not (any (parseUri u `compareDomain`) excludedDomains) = do
              $logInfo$ "Fetching: '" ++ u ++ "'"
              eitherT (\x → do
                        $logWarn x
                        return $ String u)
                      return
                      (fetch $ parseUri u)
        fetchIfAllowed x = do
          $logInfo$ "Not fetching: '" ++ tshow x ++ "'"
          return x
        fetch uri = do
          resp ← hoistEither =<< (runHTTP $ reqU uri >>= performWithHtml)
          fetchEntryWithAuthors uri resp >>= \case
            (Just (Object entry), _) → do
              prs ← lift $ fetchLinkedEntires' (depthLeft - 1) excludedDomains (insertSet uri excludedURLs) $
                fromMaybe (HMS.fromList []) $ Object entry ^? key "properties" . _Object
              right $ Object $ insertMap "properties" (Object prs) $ insertMap "fetched-url" (toJSON $ tshow uri) entry
            x → do
              left $ "Received something that's not an h-entry when parsing fetched '" ++ tshow uri ++ "'"
        excludedKeys = S.fromList [ "client-id", "content", "summary", "name", "photo", "video", "audio", "item",
                                    "syndication", "author", "category", "published", "updated",
                                    "comment", "like", "repost", "quotation", "rsvp", "mention" ]

fetchLinkedEntires ∷ (MonadHTTP ψ μ, MonadCatch μ, MonadLogger μ) ⇒ Set URI → Set URI → Object → μ Object
fetchLinkedEntires = fetchLinkedEntires' 3
