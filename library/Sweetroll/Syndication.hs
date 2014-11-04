{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sweetroll.Syndication (
  postAppDotNet
, showSyndication
) where

import           ClassyPrelude
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Web.Scotty (params)
import           Data.Microformats2
import           Data.Aeson
import           Data.Aeson.TH
import           Text.Pandoc
import           Sweetroll.Util
import           Sweetroll.Pages (renderContent)
import           Sweetroll.Conf

type SyndicationPoster = SweetrollConf -> Manager -> Entry -> SweetrollAction (Maybe LText)

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

postAppDotNet :: SyndicationPoster
postAppDotNet conf httpClientMgr entry = do
  req <- liftIO $ parseUrl ((adnApiHost conf) ++ "/posts") >>= \x -> return $ x { method = "POST", secure = True }
  let (isArticle, txt) = trimmedText 250 entry
      pUrl = fromMaybe "" $ entryUrl entry
      reqBody = encode $ object [ "text" .= if isArticle then txt else "[x] " ++ txt
                                , "annotations" .= [ object [ "type" .= asLText "net.app.core.crosspost"
                                                            , "value" .= ADNCanonicalData pUrl ] ]
                                , "entities" .= object [ "links" .= [ ADNLink 0 (if isArticle then length txt else 3) pUrl ] ] ]
  resp <- liftIO $ httpLbs (req { requestBody = RequestBodyLBS reqBody
                                , requestHeaders = [ (hAuthorization, fromString $ "Bearer " ++ (adnApiToken conf))
                                                   , (hContentType, "application/json; charset=utf-8")
                                                   , (hAccept, "application/json") ] }) httpClientMgr
  if responseStatus resp /= ok200 then return Nothing
  else do
    let respData = decode $ responseBody resp :: Maybe ADNWrapper
    return $ respData >>= return . canonical_Url . adnData

showSyndication :: SweetrollAction () -> SweetrollAction ()
showSyndication otherAction = do
  allParams <- params
  case findByKey allParams "q" of
    Just "syndicate-to" -> showXForm "syndicate-to=app.net"
    _ -> otherAction
