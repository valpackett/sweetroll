{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators, DataKinds, GADTs #-}

module Sweetroll.Microsub.Endpoint where

import           Sweetroll.Prelude
import           Web.JWT hiding (header, decode)
import           Servant
import           Data.Microformats2.Jf2
import qualified Data.HashMap.Strict as HMS
import           Sweetroll.Context
import           Sweetroll.Auth (ensureScope, getSub, mkAcl)
import           Sweetroll.Database
import           Sweetroll.Microsub.Request
import           Sweetroll.Microsub.Response
import           Sweetroll.Microsub.Fetch
import           Sweetroll.Micropub.Endpoint (setPublished, setUrl, wrapWithTypeAndAcl)

getMicrosub ∷ JWT VerifiedJWT → Maybe Text → Maybe Text → Maybe Text → Maybe Text → Maybe Text → Sweetroll MicrosubResponse
-- TODO getMicrosub token host (Just "timeline") (Just "notifications") after before = do
getMicrosub token host (Just "timeline") (Just channel) after before = do
  ensureScope token $ elem "read"
  feed ← guardEntryNotFound =<< guardDbError =<< queryDb (FeedQuery (getSub token) channel 20 before after) getFeed
  let entries = map toJf2 $ feed ^.. key "children" . values
  let paging = if isNothing after && isNothing before
                  then Just (Paging Nothing ((^? key "published" . _String) =<< lastMay entries))
                  else Just (Paging ((^? key "published" . _String) =<< headMay entries) Nothing)
  return $ Entries entries paging

getMicrosub token (Just host) (Just "channels") _ _ _ = do
  ensureScope token $ elem "read"
  Just feeds ← preview _Array <$> (guardEntryNotFound =<< guardDbError =<< queryDb ("https://" ++ host) getFeeds)
  let chans = mapMaybe compactChannel $ toList $
        filter (\x → elem "h-x-reader-channel" $ x ^? key "type" . values . _String) feeds
  return $ Channels $ Channel "notifications" "Notifications" 0 : chans

getMicrosub token host (Just "follow") (Just channel) _ _ = do
  ensureScope token $ elem "read"
  ensureRightDomain (base host) $ parseUri channel
  feed ← guardEntryNotFound =<< guardDbError =<< queryDb channel getObject
  let subs = mapMaybe compactSubscription $ toList $
        fromMaybe empty $ feed ^? key "properties" . key "subscriptions" . _Array
  return $ Subscriptions subs

getMicrosub token host action channel after before =
  throwM respNoContent


postMicrosub ∷ JWT VerifiedJWT → Maybe Text → MicrosubRequest
             → Sweetroll MicrosubResponse
postMicrosub token host (CreateChannel name) = do
  ensureScope token $ elem "channels"
  now ← liftIO getCurrentTime
  let Object props = object [ "name" .= [ name ]
                            , "category" .= [ asText "_channels" ] ]
      obj = wrapWithTypeAndAcl ["h-x-reader-channel"] [getSub token] $
        setUrl (base host) now $
        setPublished now props
  guardDbError =<< queryDb obj upsertObject
  return $ Created $ fromMaybe (Channel "" "" 0) $ compactChannel obj

postMicrosub token host (Follow channel url) = do
  ensureScope token $ elem "follow"
  guardDbError =<< queryDb (FeedSubAdd (mkAcl token) channel $ withTrailingSlash url) addSubscriptionToFeed
  fetchAndStoreFeed $ parseUri url
  return $ Subscribed $ Subscription url

postMicrosub token host (Preview url) = do
  ensureScope token $ elem "read"
  Just feed ← fetchFeed $ parseUri url
  return $ Entries (map toJf2 feed) Nothing

postMicrosub token host _ = throwM respNoContent

respNoContent ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respNoContent = ServantErr { errHTTPCode = 204
                           , errReasonPhrase = "No Content"
                           , errHeaders = [ ]
                           , errBody    = "" }

-- | Make types conform to Microsub spec (different than JF2 Feed)
ensureValidEntry ∷ Value → Value
ensureValidEntry val@(Object _) =
    (& key "author" %~ ensureValidCard) $
    extractRefs [ "like-of", "repost-of", "bookmark-of", "in-reply-to", "quotation-of", "syndication" ] $
    ensure multiple [ "category", "photo", "video", "audio", "like-of"
                    , "repost-of", "bookmark-of", "in-reply-to", "quotation-of", "syndication" ] $
    ensure single ["published", "url", "uid", "name", "content", "author"] val
  where ensure f ks v = foldl' (\acc k → acc & key k %~ f) v ks
        single (Array v) = fromMaybe Null $ headMay v
        single x = x
        multiple v@(Array _) = v
        multiple x = Array $ pure x
        extractRefs ks v@(Object _) =
          let (Object v', refs) = foldl' (\(acc, es) k →
                  (acc & key k . _Array . each %~ url,
                   foldl' (\rs v → insertMap (fromMaybe "_" $ v ^? key "url" . _String) v rs) es $
                     filter (isJust . (^? _Object)) $ acc ^.. key k . _Array . each))
                (v, HMS.empty) ks in
          Object $ insertWith concatObj "refs" (Object refs) v'
        url x@(Object _) = fromMaybe x $ x ^? key "url"
        url x = x
        concatObj (Object x) (Object y) = Object $ HMS.union x y
        concatObj x _ = x
        -- TODO embed geo/adr into direct fields
        ensureValidCard = ensure single [ "name", "url", "photo", "latitude", "longitude"
                                        , "street-address", "locality", "region", "country "]
ensureValidEntry x = x & key "children" . _Array . each %~ ensureValidEntry

toJf2 ∷ Value → Value
toJf2 = ensureValidEntry . mf2ToJf2
