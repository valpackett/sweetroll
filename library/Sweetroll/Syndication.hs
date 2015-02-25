{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE ExistentialQuantification, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Sweetroll.Syndication (
  postAppDotNet
, postTwitter
, showSyndication
) where

import           ClassyPrelude
import           Control.Lens ((^?))
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.OAuth
import           Web.Scotty.Trans (params)
import           Data.Microformats2
import qualified Data.Stringable as S
import           Data.Aeson
import           Data.Aeson.Lens
import           Text.Pandoc
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Pages (renderContent)
import           Sweetroll.Conf

trimmedText ∷ Index LText → Entry → (Bool, LText)
trimmedText l entry = (isArticle, if isTrimmed then take (l - 1) t ++ "…" else t)
  where isTrimmed = length t > fromIntegral l
        (isArticle, t) = case entryName entry of
                           n : _ → (True, n)
                           _ → (False, renderContent writePlain entry)

ifSuccess ∷ ∀ (m ∷ * → *) body a. Monad m ⇒ Response body → Maybe a → m (Maybe a)
ifSuccess resp what = return $ if not $ statusIsSuccessful $ responseStatus resp then Nothing else what

postAppDotNet ∷ (MonadSweetroll m, MonadIO m) ⇒ Entry → m (Maybe LText)
postAppDotNet entry = do
  req ← parseUrlP "/posts" =<< getConfOpt adnApiHost
  bearer ← getSec adnApiToken
  let (isArticle, txt) = trimmedText 250 entry
      pUrl = orEmpty . entryUrl $ entry
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

appDotNetUrl ∷ (S.Stringable s) ⇒ Maybe Value → Maybe s
appDotNetUrl x = (return . S.fromText) =<< (^? key "data" . key "canonical_url" . _String) =<< x

postTwitter ∷ (MonadSweetroll m, MonadIO m) ⇒ Entry → m (Maybe LText)
postTwitter entry = do
  req ← parseUrlP "/statuses/update.json" =<< getConfOpt twitterApiHost
  secs ← getSecs
  let (_, txt) = trimmedText 100 entry -- TODO: Figure out the number based on mentions of urls/domains in the first (140 - 25) characters
      pUrl = orEmpty . entryUrl $ entry
      reqBody = writeForm [("status", txt ++ " " ++ pUrl)]
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
tweetUrl ∷ (S.Stringable s) ⇒ Maybe Value → Maybe s
tweetUrl root' = do
  root ← root'
  tweetId ← root ^? key "id_str" . _String
  tweetUsr ← root ^? key "user" . key "screen_name" . _String
  return $ S.fromText $ mconcat ["https://twitter.com/", tweetUsr, "/status/", tweetId]

showSyndication ∷ SweetrollAction () → SweetrollAction ()
showSyndication otherAction = do
  allParams ← params
  case findByKey allParams "q" of
    Just "syndicate-to" → showForm $ map (\x → ("syndicate-to[]", asByteString x)) ["app.net", "twitter.com"]
    _ → otherAction
