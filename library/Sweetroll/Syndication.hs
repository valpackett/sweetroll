{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Sweetroll.Syndication (
  postAppDotNet
, postTwitter
, showSyndication
) where

import           ClassyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.OAuth
import "crypto-random" Crypto.Random
import           Web.Scotty (params)
import           Data.Microformats2
import           Data.Aeson
import           Data.Aeson.TH
import           Text.Pandoc
import           Sweetroll.Util
import           Sweetroll.Pages (renderContent)
import           Sweetroll.Conf

trimmedText :: Index LText -> Entry -> (Bool, LText)
trimmedText l entry = (isArticle, if isTrimmed then (take (l - 1) t) ++ "…" else t)
  where isTrimmed = length t > fromIntegral l
        (isArticle, t) = case entryName entry of
                           Just n -> (True, n)
                           _ -> (False, renderContent writePlain entry)

data ADNCanonicalData = ADNCanonicalData { canonical_Url :: LText }
$(deriveJSON defaultOptions { fieldLabelModifier = toLower } ''ADNCanonicalData)
data ADNWrapper = ADNWrapper { adnData :: ADNCanonicalData }
$(deriveJSON defaultOptions { fieldLabelModifier = toLower . drop 3 } ''ADNWrapper)
data ADNLink = ADNLink { _adnLinkPos :: Int, _adnLinkLen :: Int, _adnLinkUrl :: LText }
$(deriveJSON defaultOptions { fieldLabelModifier = toLower . drop 8 } ''ADNLink)

postAppDotNet :: SweetrollConf -> Manager -> Entry -> SweetrollAction (Maybe LText)
postAppDotNet conf mgr entry = do
  req <- liftIO $ parseUrl $ (adnApiHost conf) ++ "/posts"
  let (isArticle, txt) = trimmedText 250 entry
      pUrl = fromMaybe "" $ entryUrl entry
      reqBody = encode $ object [ "text" .= if isArticle then txt else "[x] " ++ txt
                                , "annotations" .= [ object [ "type" .= asLText "net.app.core.crosspost"
                                                            , "value" .= ADNCanonicalData pUrl ] ]
                                , "entities" .= object [ "links" .= [ ADNLink 0 (if isArticle then length txt else 3) pUrl ] ] ]
      req' = req { method = "POST"
                 , requestBody = RequestBodyLBS reqBody
                 , requestHeaders = [ (hAuthorization, fromString $ "Bearer " ++ (adnApiToken conf))
                                    , (hContentType, "application/json; charset=utf-8")
                                    , (hAccept, "application/json") ] }
  resp <- liftIO $ httpLbs req' mgr
  if not $ statusIsSuccessful $ responseStatus resp then return Nothing
  else do
    let respData = decode $ responseBody resp :: Maybe ADNWrapper
    return $ canonical_Url <$> adnData <$> respData

data TwitterUser = TwitterUser { twitterScreen_Name :: LText }
$(deriveJSON defaultOptions { fieldLabelModifier = toLower . drop 7 } ''TwitterUser)
data TwitterPost = TwitterPost { twitterId_Str :: LText, twitterUser :: TwitterUser }
$(deriveJSON defaultOptions { fieldLabelModifier = toLower . drop 7 } ''TwitterPost)

postTwitter :: SweetrollConf -> Manager -> SystemRNG -> Entry -> SweetrollAction (Maybe LText)
postTwitter conf mgr rng entry = do
  req <- liftIO $ parseUrl $ (twitterApiHost conf) ++ "/statuses/update.json"
  let (_, txt) = trimmedText 100 entry -- TODO: Figure out the number based on mentions of urls/domains in the first (140 - 25) characters
      pUrl = fromMaybe "" $ entryUrl entry
      reqBody = writeForm [("status", txt ++ " " ++ pUrl)]
      req' = req { method = "POST"
                 , queryString = reqBody -- Yes, queryString... WTF http://ox86.tumblr.com/post/36810273719/twitter-api-1-1-responds-with-status-401-code-32
                 , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8")
                                    , (hAccept, "application/json") ] }
      accessToken = Token (twitterAccessToken conf) (twitterAccessSecret conf)
      clientCreds = clientCred $ Token (twitterAppKey conf) (twitterAppSecret conf)
      creds = permanentCred accessToken clientCreds
  (signedReq, _rng) <- liftIO $ oauth creds defaultServer req' rng
  resp <- liftIO $ httpLbs signedReq mgr
  if not $ statusIsSuccessful $ responseStatus resp then return Nothing
  else do
    let respData = decode $ responseBody resp :: Maybe TwitterPost
    return $ do
      tweetId <- twitterId_Str <$> respData
      tweetUsr <- twitterScreen_Name . twitterUser <$> respData
      return $ mconcat ["https://twitter.com/", tweetUsr, "/status/", tweetId] -- Y U NO GIVE ME THE LINK

showSyndication :: SweetrollAction () -> SweetrollAction ()
showSyndication otherAction = do
  allParams <- params
  case findByKey allParams "q" of
    Just "syndicate-to" -> showForm [("syndicate-to", asByteString "app.net,twitter.com")]
    _ -> otherAction
