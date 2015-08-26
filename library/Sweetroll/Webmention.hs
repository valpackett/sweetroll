{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, RankNTypes, TupleSections #-}

module Sweetroll.Webmention where

import           ClassyPrelude
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import           Data.Conduit
import           Data.Aeson
import           Data.List (nub)
import           Data.Stringable
import           Data.Microformats2.Parser
import           Data.IndieWeb.Endpoints
import           Network.HTTP.Link
import           Network.HTTP.Types
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client.Conduit
import           Network.URI
import           Sweetroll.Util
import           Sweetroll.Conf
import           Sweetroll.Monads

linksFromHeader ∷ ∀ body. Response body → [Link]
linksFromHeader r = fromMaybe [] (lookup "Link" (responseHeaders r) >>= parseLinkHeader . decodeUtf8)

discoverWebmentionEndpoints ∷ Value → [Link] → [URI]
discoverWebmentionEndpoints = discoverEndpoints [ "webmention", "http://webmention.org/" ]

getWebmentionEndpoint ∷ Response (Source Sweetroll ByteString) → Sweetroll (Maybe URI)
getWebmentionEndpoint r = do
  htmlDoc ← responseBody r $$ sinkDoc
  let mf2Root = parseMf2 mf2Options $ documentRoot htmlDoc
  return $ listToMaybe $ discoverWebmentionEndpoints mf2Root (linksFromHeader r)

sendWebmention ∷ String → URI → URI → Sweetroll ()
sendWebmention from to endpoint = do
  req ← setUri def endpoint
  let reqBody = writeForm [(asText "source", from), ("target", show to)]
      req' = req { method = "POST"
                 , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                 , requestBody = RequestBodyBS reqBody }
  void $ withSuccessfulRequest req' $ \_ → do
    putStrLn $ toText $ "Webmention posted for <" ++ show to ++ ">!"
    return $ Just ()

contentWebmentions ∷ Maybe P.Pandoc → Sweetroll [(URI, URI)]
contentWebmentions content =
  case content of
    Nothing → return []
    Just p → do
      let extractLink (P.Link _ (u, _)) = catMaybes [ parseURI u ]
          extractLink _ = []
          links = PW.query extractLink p
          getWebmentionEndpoint' uri = do
            endp ← withSuccessfulRequestHtml uri getWebmentionEndpoint
            return $ (uri, ) <$> endp
      rs ← mapM getWebmentionEndpoint' links
      return $ catMaybes rs

sendWebmentions ∷ String → [(URI, URI)] → Sweetroll ()
sendWebmentions from ms = mapM_ (uncurry $ sendWebmention from) $ nub ms
