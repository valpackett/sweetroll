{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Sweetroll.Webmention.Send where

import           Sweetroll.Prelude hiding (from, to)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import           Data.Microformats2.Parser
import           Data.IndieWeb.Endpoints
import           Network.HTTP.Link
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.HTTPClient hiding (Header)

type SourceURI = URI
type TargetURI = URI
type EndpointURI = URI

sendWebmention ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                 SourceURI → TargetURI → EndpointURI → μ (Maybe (Response LByteString))
sendWebmention from to endpoint = do
  resp ← runHTTP $ reqU endpoint >>= anyStatus
                 >>= postForm [ ("source", tshow from), ("target", tshow to) ]
                 >>= performWithBytes
  let result = case resp of
                 Left e → "error: " ++ e
                 Right _ → "success"
  putStrLn $ "Webmention sending result for <" ++ tshow to ++ ">: " ++ result ++ "!"
  return $ hush resp

sendWebmentions ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                  SourceURI → [(TargetURI, EndpointURI)] → μ [Maybe (Response LByteString)]
sendWebmentions from ms = mapM (uncurry $ sendWebmention from) $ nub ms


linksFromHeader ∷ ∀ body. Response body → [Link]
linksFromHeader r = fromMaybe [] (lookup "Link" (responseHeaders r) >>= parseLinkHeader . decodeUtf8)

discoverWebmentionEndpoints ∷ Value → [Link] → [EndpointURI]
discoverWebmentionEndpoints = discoverEndpoints [ "webmention", "http://webmention.org/" ]

getWebmentionEndpoint ∷ Response XDocument → Maybe EndpointURI
getWebmentionEndpoint r = listToMaybe $ discoverWebmentionEndpoints mf2Root (linksFromHeader r)
    where mf2Root = parseMf2 mf2Options $ documentRoot $ responseBody r

findWebmentionEndpoints ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                          [TargetURI] → μ [(TargetURI, EndpointURI)]
findWebmentionEndpoints targets = do
  let getWebmentionEndpoint' uri = do
        resp ← runHTTP $ reqU uri >>= anyStatus >>= performWithHtml
        return $ liftM (uri, ) $ getWebmentionEndpoint =<< (hush resp)
  rs ← mapM getWebmentionEndpoint' targets
  return $ catMaybes rs

contentWebmentions ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
                     Maybe P.Pandoc → μ [(TargetURI, EndpointURI)]
contentWebmentions Nothing = return []
contentWebmentions (Just p) = do
  let extractLink (P.Link _ _ (u, _)) = catMaybes [ parseURI u ]
      extractLink _ = []
      links = PW.query extractLink p
  findWebmentionEndpoints links
