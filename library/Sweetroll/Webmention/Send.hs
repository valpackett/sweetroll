{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Sweetroll.Webmention.Send where

import           ClassyPrelude
import           Control.Monad.Trans.Control
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Aeson
import           Data.List (nub)
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

type SourceURI = URI
type TargetURI = URI
type EndpointURI = URI

sendWebmention ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                 SourceURI → TargetURI → EndpointURI → μ (Maybe (Response LByteString))
sendWebmention from to endpoint = do
  req ← setUri def endpoint
  let reqBody = writeForm [(asText "source", tshow from), ("target", tshow to)]
      req' = req { method = "POST"
                 , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                 , requestBody = RequestBodyBS reqBody }
  withSuccessfulRequest req' $ \resp → do
    putStrLn $ "Webmention posted for <" ++ tshow to ++ ">!"
    body ← responseBody resp $$ C.sinkLazy
    return $ Just $ resp { responseBody = body }

sendWebmentions ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                  SourceURI → [(TargetURI, EndpointURI)] → μ [Maybe (Response LByteString)]
sendWebmentions from ms = mapM (uncurry $ sendWebmention from) $ nub ms


linksFromHeader ∷ ∀ body. Response body → [Link]
linksFromHeader r = fromMaybe [] (lookup "Link" (responseHeaders r) >>= parseLinkHeader . decodeUtf8)

discoverWebmentionEndpoints ∷ Value → [Link] → [EndpointURI]
discoverWebmentionEndpoints = discoverEndpoints [ "webmention", "http://webmention.org/" ]

getWebmentionEndpoint ∷ (MonadThrow μ, MonadSweetroll μ) ⇒
                        Response (Source μ ByteString) → μ (Maybe EndpointURI)
getWebmentionEndpoint r = do
  htmlDoc ← responseBody r $$ sinkDoc
  let mf2Root = parseMf2 mf2Options $ documentRoot htmlDoc
  return $ listToMaybe $ discoverWebmentionEndpoints mf2Root (linksFromHeader r)

findWebmentionEndpoints ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                          [TargetURI] → μ [(TargetURI, EndpointURI)]
findWebmentionEndpoints targets = do
  let getWebmentionEndpoint' uri = do
        endp ← withSuccessfulRequestHtml uri getWebmentionEndpoint
        return $ (uri, ) <$> endp
  rs ← mapM getWebmentionEndpoint' targets
  return $ catMaybes rs

contentWebmentions ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                     Maybe P.Pandoc → μ [(TargetURI, EndpointURI)]
contentWebmentions Nothing = return []
contentWebmentions (Just p) = do
  let extractLink (P.Link _ (u, _)) = catMaybes [ parseURI u ]
      extractLink _ = []
      links = PW.query extractLink p
  findWebmentionEndpoints links
