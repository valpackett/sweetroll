{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts, ConstraintKinds, LambdaCase #-}

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
import           Sweetroll.Context
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

performWithHtml ∷ Request → ExceptT Text Sweetroll (Response XDocument)
performWithHtml = performWithFn (.| sinkDoc) . (\req → req { requestHeaders = [ (hAccept, "text/html; charset=utf-8") ] })

fetchEntriesWithAuthors ∷ URI → Response XDocument → ExceptT Text Sweetroll ([Value], Value)
fetchEntriesWithAuthors uri res = do
  let mf2Options' = mf2Options { baseUri = Just uri }
      mfRoot = parseMf2 mf2Options' $ documentRoot $ responseBody res
      allMfs = fromMaybe [] $ allMicroformatsOfType "h-entry" mfRoot
        <|> (map (\x → [(x, [])]) $ parseTwitter mf2Options' $ documentRoot $ responseBody res)
  he ← forM allMfs $ \mfEntry@(mfE, _) → do
      let fetch uri' = fmap (\x → responseBody <$> hush x) $ runHTTP $ reqU uri' >>= performWithHtml
      authors ← lift $ entryAuthors mf2Options' fetch uri mfRoot mfEntry
      let addAuthors (Object o) = Object $ adjust addAuthors' "properties" o
          addAuthors x = x
          addAuthors' (Object o) = Object $ insertMap "author" (Array $ fromList $ fromMaybe [] authors) o
          addAuthors' x = x
      return $ addAuthors mfE
  return (he, mfRoot)

fetchEntryWithAuthors ∷ URI → Response XDocument → ExceptT Text Sweetroll (Maybe Value, Value)
fetchEntryWithAuthors uri res = (& _1 %~ headMay) <$> fetchEntriesWithAuthors uri res

fetchLinkedEntires' ∷ Int → Set URI → Set URI → Object → Sweetroll Object
fetchLinkedEntires' 0 _ _ props = return props
fetchLinkedEntires' depthLeft excludedDomains excludedURLs props = do
  (flip HMS.traverseWithKey) props $ curry $ \case
    (k, (Array v)) | k `notElem` excludedKeys → Array <$> forM v fetchIfAllowed
    (_, v) → return v
  where fetchIfAllowed v@(Object o) = maybe (do
              logInfo $ "Could not extract URL from object for fetching: " ++ display (tshow o)
              return v)
            (fetchIfAllowed . String)
            (v ^? key "properties" . key "url" . nth 0 . _String)
        fetchIfAllowed (String u) | isNothing (parseURI $ cs u) = do
          logInfo $ "Not a parsable URL for fetching: '" ++ display u ++ "'"
          return (String u)
        fetchIfAllowed (String u)
          | isJust (parseURI $ cs u)
            && parseUri u `notElem` excludedURLs
            && not (any (parseUri u `compareDomain`) excludedDomains) = do
              logInfo $ "Fetching: '" ++ display u ++ "'"
              exceptT (\x → do
                        logWarn $ display x
                        return $ String u)
                      return
                      (fetch $ parseUri u)
        fetchIfAllowed x = do
          logInfo $ "Not fetching: '" ++ display (tshow x) ++ "'"
          return x
        fetch uri = do
          resp ← hoistEither =<< (lift $ runHTTP $ reqU uri >>= performWithHtml)
          fetchEntryWithAuthors uri resp >>= \case
            (Just (Object entry), _) → do
              prs' ← lift $ fetchLinkedEntires' (depthLeft - 1) excludedDomains (insertSet uri excludedURLs) $
                fromMaybe (HMS.fromList []) $ Object entry ^? key "properties" . _Object
              -- additional filtering to prevent the author from overriding a local entry
              let (Object prs) = Object prs' & (deep $ filtered shouldExclude) %~ (\x → fromMaybe "" $ x ^? key "properties" . key "url" . _Array . each)
              return $ Object $ insertMap "properties" (Object prs) $ insertMap "fetched-url" (toJSON $ tshow uri) entry
            x → throwE $ "Received something that's not an h-entry when parsing fetched '" ++ tshow uri ++ "'"
        excludedKeys = S.fromList [ "client-id", "content", "summary", "name", "photo", "video", "audio", "item",
                                    "syndication", "author", "category", "published", "updated",
                                    "comment", "like", "repost", "quotation", "rsvp", "mention" ]
        shouldExclude x = fromMaybe False $ (\u → any (u `compareDomain`) excludedDomains) <$> (parseURI =<< cs <$> x ^? key "properties" . key "url" . _Array . each . _String)

fetchLinkedEntires ∷ Set URI → Set URI → Object → Sweetroll Object
fetchLinkedEntires = fetchLinkedEntires' 3
