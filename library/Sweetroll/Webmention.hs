{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Sweetroll.Webmention (
  discoverWebmentionEndpoint
, sendWebmention
, sendWebmentions
) where

import           ClassyPrelude
import           System.IO.Unsafe (unsafePerformIO)
import           Text.XML.HXT.Core hiding (trace)
import           Text.XML.HXT.TagSoup
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import           Control.Concurrent.Async
import           Data.Microformats2
import           Data.Foldable (asum)
import qualified Data.Set as S
import           Network.HTTP.Link
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.URI
import           Sweetroll.Util

hLink :: HeaderName
hLink = "Link"

isWebmentionRel :: (EqSequence seq, IsString seq) => seq -> Bool
isWebmentionRel = isInfixOf "webmention"

-- AWESOME: this is probably my favorite function ever
-- MAYBE: properly resolve URLs relative to <base>
-- | Discovers a webmention endpoint for an address.
discoverWebmentionEndpoint :: Response LByteString -> URI -> Maybe URI
discoverWebmentionEndpoint r to = asum [ lnk >>= parseAbsoluteURI
                                       , lnk >>= parseRelativeReference >>= return . (`relativeTo` to) ]
  where lnk = asum [findInHeader, findInBody]
        findInHeader = (lookup hLink $ responseHeaders r)
                       >>= parseLinkHeader . decodeUtf8
                       >>= find (isWebmentionRel . fromMaybe "" . lookup Rel . linkParams)
                       >>= return . unpack . href
        findInBody = listToMaybe $ unsafePerformIO $ runX $
                       htmlDoc //> hasAttrValue "rel" isWebmentionRel >>> getAttrValue "href"
        htmlDoc = readString [withTagSoup] $ unpack $ decodeUtf8 $ toStrict $ responseBody r

-- | Sends one single webmention.
sendWebmention :: Manager -> String -> String -> IO (String, Bool)
sendWebmention mgr from to = do
  tReq <- parseUrl to
  tResp <- httpLbs tReq mgr
  let endp = discoverWebmentionEndpoint tResp $ getUri tReq
  case endp of
    Just u -> do
      eReq <- parseUrl $ uriToString id u ""
      let reqBody = writeForm [("from", from), ("to", to)]
      eResp <- httpLbs eReq { method = "POST"
                            , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                            , requestBody = RequestBodyBS reqBody } mgr
      return $ (to, responseStatus eResp == ok200 || responseStatus eResp == accepted202)
    _ -> return (to, False)

-- | Send all webmentions required for an entry, including the ones from
-- metadata (in-reply-to, like-of, repost-of).
sendWebmentions :: Manager -> Entry -> IO [(String, Bool)]
sendWebmentions mgr e = mapConcurrently (sendWebmention mgr from) $ S.toList links
  where links = S.fromList $ contentLinks ++ metaLinks
        metaLinks = map unpack $ catMaybes $ map derefEntry $ catMaybes [entryInReplyTo e, entryLikeOf e, entryRepostOf e]
        contentLinks = PW.query extractLink $ pandocContent $ entryContent e
        from = unpack $ fromMaybe "" $ entryUrl e
        pandocContent (Just (Left p)) = p
        pandocContent (Just (Right t)) = P.readMarkdown pandocReaderOptions $ unpack t
        pandocContent _ = P.readMarkdown pandocReaderOptions ""
        extractLink (P.Link _ (u, _)) = [u]
        extractLink _ = []
