{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

-- | A reverse proxy for images included in responses that protects readers' privacy and prevents TLS mixed content warnings.

module Sweetroll.Proxy (
  signUrlForProxy
, requestProxy
, proxiedUri
, proxyImages
) where

import           Sweetroll.Prelude
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.ByteArray.Encoding
import           Data.Conduit
import           Text.XML.Lens
import           Network.Wai.Conduit as W
import           Network.HTTP.Conduit as HC
import           Network.HTTP.Client.Conduit as HCC
import           Sweetroll.Conf
import           Sweetroll.Monads

signature ∷ (ConvertibleStrings α ByteString) ⇒ SweetrollSecrets → α → HMAC Blake2sp_256
signature secs u = hmac (proxySigningKey secs) $ asByteString $ cs u

decodeSignature ∷ (ConvertibleStrings α ByteString) ⇒ α → Maybe (HMAC Blake2sp_256)
decodeSignature s = HMAC <$> (digestFromByteString =<< decoded)
  where decoded = map asByteString $ hush $ convertFromBase Base64URLUnpadded $ asByteString $ cs s

signUrlForProxy ∷ (ConvertibleStrings α ByteString, ConvertibleStrings ByteString β)
                ⇒ SweetrollSecrets → α → β
signUrlForProxy secs = cs . asByteString . convertToBase Base64URLUnpadded . signature secs

requestProxy ∷ SweetrollCtx → Application
requestProxy ctx req respond =
  case (join $ lookup "url" $ W.queryString req, join $ lookup "sig" $ W.queryString req) of
    (Just url, Just sig) | (Just $ signature (_ctxSecs ctx) url) /= decodeSignature sig →
      respond $ responseLBS badRequest400 [("Content-Type", "text/plain")] "sig doesn't match"
    (Just url, Just _) → do
      upstreamReq0 ← parseUrl $ cs url
      let upstreamReq = upstreamReq0 { HCC.requestHeaders = filter allowedReqHeader (W.requestHeaders req)
                                     , HCC.decompress = const False }
      flip runReaderT ctx $ withResponse upstreamReq $ \upstreamRsp → lift $ do
        case fromMaybe 0 $ readMay =<< (asString . cs <$> lookup "Content-Length" (HCC.responseHeaders upstreamRsp)) of
          l | l > (4*1024*1024 ∷ Int) → respond $ responseLBS badRequest400 [("Content-Type", "text/plain")] "content-length too big"
          _ → do
            let src = mapOutput (Chunk . fromByteString) (HCC.responseBody upstreamRsp)
                headers = filter allowedRespHeader $ HCC.responseHeaders upstreamRsp
            respond $ responseSource (HCC.responseStatus upstreamRsp) headers src
    _ →
      respond $ responseLBS badRequest400 [("Content-Type", "text/plain")] "url and sig params must be present"
  where allowedReqHeader (h, _) = h `elem` [ "Accept-Encoding", "Accept" ]
        allowedRespHeader (h, _) = h `elem` [ "Content-Encoding", "Content-Length", "Content-Range", "Content-Type", "Date", "ETag", "Last-Modified", "Expires", "Transfer-Encoding", "Vary", "X-Content-Duration", "X-Content-Type-Options" ]

proxiedUri ∷ SweetrollSecrets → Text → Text
proxiedUri secs s = "/proxy?" ++ writeForm [(asText "url", s), ("sig", signUrlForProxy secs s)]

proxyImages ∷ SweetrollSecrets → SweetrollConf → XElement → XElement
proxyImages secs _ e = e & entire . el "img" . attribute "src" %~ makeProxied
  where makeProxied (Just s) | not ("/" `isPrefixOf` s) = Just $ proxiedUri secs s
        makeProxied x = x
