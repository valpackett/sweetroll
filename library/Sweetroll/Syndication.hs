{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE ExistentialQuantification, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Sweetroll.Syndication (
  postAppDotNet
, postTwitter
, getSyndication
) where

import           ClassyPrelude
import           Control.Lens hiding (Index, re, parts, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.OAuth
import qualified Data.Stringable as S
import           Data.Aeson
import           Data.Aeson.Lens
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Conf

ifSuccess ∷ ∀ (μ ∷ * → *) body α. Monad μ ⇒ Response body → Maybe α → μ (Maybe α)
ifSuccess resp what = return $ if not $ statusIsSuccessful $ responseStatus resp then Nothing else what

postAppDotNet ∷ Value → Sweetroll (Maybe LText)
postAppDotNet entry = do
  req ← parseUrlP "/posts" =<< getConfOpt adnApiHost
  bearer ← getSec adnApiToken
  let (isArticle, txt) = trimmedText 250 entry
      pUrl = orEmptyMaybe $ entry ^? key "properties" . key "url" . nth 0 . _String
      o = object
      reqData = o [ "text" .= if isArticle then txt else "[x] " ++ txt
                  , "annotations" .= [ o [ "type" .= asLText "net.app.core.crosspost"
                                         , "value" .= o [ "canonical_url" .= pUrl ] ] ]
                  , "entities" .= o [ "links" .= [ o [ "pos" .= (0 ∷ Int)
                                                     , "len" .= (if isArticle then length txt else 3)
                                                     , "url" .= pUrl ] ] ] ]
      req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ encode reqData
                 , requestHeaders = [ (hAuthorization, fromString $ "Bearer " ++ bearer)
                                    , (hContentType, "application/json; charset=utf-8")
                                    , (hAccept, "application/json") ] }
  resp ← request req'
  putStrLn $ "App.net status for <" ++ S.toText pUrl ++ ">: " ++ (S.toText . show . statusCode $ responseStatus resp)
  ifSuccess resp $ appDotNetUrl $ decode $ responseBody resp

appDotNetUrl ∷ (S.Stringable α) ⇒ Maybe Value → Maybe α
appDotNetUrl x = (return . S.fromText) =<< (^? key "data" . key "canonical_url" . _String) =<< x

postTwitter ∷ Value → Sweetroll (Maybe LText)
postTwitter entry = do
  req ← parseUrlP "/statuses/update.json" =<< getConfOpt twitterApiHost
  secs ← getSecs
  let (_, txt) = trimmedText 100 entry -- TODO: Figure out the number based on mentions of urls/domains in the first (140 - 25) characters
      pUrl = orEmptyMaybe $ entry ^? key "properties" . key "url" . nth 0 . _String
      reqBody = writeForm [(asText "status", txt ++ " " ++ S.toLazyText pUrl)]
      req' = req { method = "POST"
                 , queryString = reqBody -- Yes, queryString... WTF http://ox86.tumblr.com/post/36810273719/twitter-api-1-1-responds-with-status-401-code-32
                 , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8")
                                    , (hAccept, "application/json") ] }
      accessToken = Token (twitterAccessToken secs) (twitterAccessSecret secs)
      clientCreds = clientCred $ Token (twitterAppKey secs) (twitterAppSecret secs)
      creds = permanentCred accessToken clientCreds
  (signedReq, _rng) ← liftIO . oauth creds defaultServer req' =<< getRng
  resp ← request signedReq
  putStrLn $ "Twitter status for <" ++ S.toText pUrl ++ ">: " ++ (S.toText . show . statusCode $ responseStatus resp)
  ifSuccess resp $ tweetUrl $ decode $ responseBody resp

-- | Constructs a tweet URL from tweet JSON.
--
-- >>> (tweetUrl $ decode $ fromString "{\"id_str\": \"1234\", \"user\": {\"screen_name\": \"username\"}}") :: Maybe String
-- Just "https://twitter.com/username/status/1234"
tweetUrl ∷ (S.Stringable α) ⇒ Maybe Value → Maybe α
tweetUrl root' = do
  root ← root'
  tweetId ← root ^? key "id_str" . _String
  tweetUsr ← root ^? key "user" . key "screen_name" . _String
  return $ S.fromText $ mconcat ["https://twitter.com/", tweetUsr, "/status/", tweetId]

getSyndication ∷ Sweetroll [(Text, Text)]
getSyndication = return $ map (\x → ("syndicate-to[]", x)) ["app.net", "twitter.com"]
