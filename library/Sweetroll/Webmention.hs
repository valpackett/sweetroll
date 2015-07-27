{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Sweetroll.Webmention (
  discoverWebmentionEndpoint
, sendWebmention
, sendWebmentions
) where

import           ClassyPrelude
import           Text.HTML.DOM
import           Text.XML.Lens hiding (to, from)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import           Data.Conduit
import           Data.Microformats2
import           Data.Foldable (asum)
import           Data.Stringable (toText)
import qualified Data.Set as S
import           Network.HTTP.Link
import           Network.HTTP.Types
import           Network.HTTP.Client.Conduit
import           Network.URI
import           Sweetroll.Util
import           Sweetroll.Monads

hLink ∷ HeaderName
hLink = "Link"

isWebmentionRel ∷ (EqSequence α, IsString α) ⇒ α → Bool
isWebmentionRel = isInfixOf "webmention"

-- | Discovers a webmention endpoint for an address.
discoverWebmentionEndpoint ∷ URI → Response (Source Sweetroll ByteString) → Sweetroll (Maybe URI)
discoverWebmentionEndpoint to r = do
  htmlDoc ← responseBody r $$ sinkDoc
  let findInHeader = lookup hLink (responseHeaders r)
                     >>= parseLinkHeader . decodeUtf8
                     >>= find (isWebmentionRel . fromMaybe "" . lookup Rel . linkParams)
                     >>= return . show . href
      findInBody = unpack <$> htmlDoc ^. root . entire ./ attributeSatisfies "rel" isWebmentionRel . attribute "href"
      baseInBody = parseAbsoluteURI =<< unpack <$> htmlDoc ^. root . entire ./ el "base" . attribute "href"
      result = asum [findInHeader, findInBody]
      base = fromMaybe to baseInBody
  return $ asum [ result >>= parseAbsoluteURI
                , result >>= parseRelativeReference >>= return . (`relativeTo` base) ]

-- | Sends one single webmention.
sendWebmention ∷ String → String → Sweetroll (String, Bool)
sendWebmention from to = do
  tReq ← parseUrl to
  endp ← withResponse tReq $ discoverWebmentionEndpoint $ getUri tReq
  case endp of
    Just u → do
      eReq ← liftIO $ parseUrl $ uriToString id u ""
      let reqBody = writeForm [(asText "source", from), ("target", to)]
      eResp ← request eReq { method = "POST"
                           , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                           , requestBody = RequestBodyBS reqBody } ∷ Sweetroll (Response String)
      putStrLn $ "Webmention status for <" ++ (asText . pack $ to) ++ ">: " ++ (toText . show . statusCode $ responseStatus eResp)
      return (to, responseStatus eResp == ok200 || responseStatus eResp == accepted202)
    _ → do
      putStrLn $ "No webmention endpoint found for <" ++ (asText . pack $ to) ++ ">"
      return (to, False)

-- | Send all webmentions required for an entry, including the ones from
-- metadata (in-reply-to, like-of, repost-of).
sendWebmentions ∷ Entry → Sweetroll [(String, Bool)]
sendWebmentions e = mapM (sendWebmention from) links
  where links = S.toList . S.fromList $ contentLinks ++ metaLinks
        metaLinks = map unpack . mapMaybe derefEntry . mapMaybe headMay $ [entryInReplyTo e, entryLikeOf e, entryRepostOf e]
        contentLinks = PW.query extractLink . pandocContent $ entryContent e
        from = unpack . orEmpty . entryUrl $ e
        pandocContent (PandocContent p : _) = p
        pandocContent (TextContent t   : _) = pandocRead P.readMarkdown $ unpack t
        pandocContent _ = pandocRead P.readMarkdown ""
        extractLink (P.Link _ (u, _)) = [u]
        extractLink _ = []
