{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Sweetroll.Microsub.Fetch where

import qualified Control.Concurrent.MSem as MS
import           Sweetroll.Prelude
import           Sweetroll.Conf
import           Sweetroll.Context
import           Sweetroll.Database
import           Sweetroll.HTTPClient hiding (Header)

fetchFeed ∷ URI → Sweetroll (Maybe [Value])
fetchFeed url = do
  resp0 ← runHTTP $ reqU url >>= anyStatus >>= performWithHtml
  case resp0 of
    Left e → do
      logInfo $ "Error fetching feed " ++ display (tshow url) ++ ": " ++ display e
      return Nothing
    Right resp → do
      case statusCode $ responseStatus resp of
        -- 410 → do
        200 → do
          (entries, _) ← exceptT
            (\e → logError (display e) >> return ([], Null))
            return
            (fetchEntriesWithAuthors url resp)
          logInfo $ "Received " ++ display (length entries) ++ " entries feed " ++ display (tshow url)
          return $ Just entries
        x → do
          logInfo $ "Received status code " ++ display x ++ " when fetching feed " ++ display (tshow url)
          return Nothing

fetchAndStoreFeed ∷ URI → Sweetroll ()
fetchAndStoreFeed url = do
  feed0 ← fetchFeed url
  case feed0 of
    Nothing → logInfo "Cannot store feed that wasn't fetched"
    Just entries → do
      forM_ entries $ \entry → do
        guardDbError =<< queryDb entry upsertObject
      let entryUrls = mapMaybe (^? key "properties" . key "url" . nth 0 . _String) entries
      logInfo $ "Received entry URLs: " ++ display (tshow entryUrls)
      guardDbError =<< queryDb (FeedEntryAdd (withTrailingSlash $ tshow url) entryUrls) addEntriesToFeeds


fetchAndStoreAllFeeds ∷ Sweetroll ()
fetchAndStoreAllFeeds = do
  urls ← guardDbError =<< queryDb () getSubscriptionUrls
  limiter ← liftIO $ MS.new 8 -- max concurrency
  _ ← forConcurrently (catMaybes urls) $ \url → withRunInIO $ \runi → MS.with limiter $ runi $ do
    logInfo $ "Going to fetch feed: " ++ display url
    fetchAndStoreFeed $ parseUri url
  return ()

fetchAndStoreAllFeedsIfAllowed ∷ Maybe Text → Sweetroll NoContent
fetchAndStoreAllFeedsIfAllowed token = do
  secs ← getSecs
  if token == Just (pollSecretToken secs)
     then async fetchAndStoreAllFeeds >> return NoContent
     else throwM respUnauthorized

respUnauthorized ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respUnauthorized = ServantErr { errHTTPCode = 401
                              , errReasonPhrase = "Unauthorized"
                              , errHeaders = [ ]
                              , errBody    = "" }
