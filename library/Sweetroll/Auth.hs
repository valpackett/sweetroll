{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

-- | The IndieAuth/rel-me-auth implementation, using JSON Web Tokens.
module Sweetroll.Auth (
  checkAuth
, showAuth
, makeAccessToken
, doIndieAuth
) where

import           ClassyPrelude
import           Data.Time.Clock.POSIX
import           Web.Scotty hiding (request)
import           Web.JWT hiding (header)
import           Network.HTTP.Types.Status
import           Network.HTTP.Client
import           Sweetroll.Util
import           Sweetroll.Conf

getAccessToken :: SweetrollAction Text
getAccessToken = do
  allParams <- params
  tokenHeader <- header "Authorization" >>= \x -> return $ fromMaybe "" $ drop 7 <$> x -- Drop "Bearer "
  return $ toStrict $ fromMaybe tokenHeader $ findByKey allParams "access_token"

checkAuth :: (?conf :: SweetrollConf) => SweetrollAction () -> SweetrollAction () -> SweetrollAction ()
checkAuth onFail act = do
  token <- getAccessToken
  let verResult = decodeAndVerifySignature (secret $ secretKey ?conf) token
  case verResult of
    Just _ -> act
    _ -> onFail

showAuth :: SweetrollAction ()
showAuth = do
  token <- getAccessToken
  case map claims $ decode token of
    Just cs ->
      showForm [("me", meVal)]
          where meVal = case sub cs of
                          Just me -> show me
                          _ -> ""
    _ -> status unauthorized401

makeAccessToken :: SweetrollConf -> Text -> SweetrollAction ()
makeAccessToken conf me = do
  now <- liftIO getCurrentTime
  let t = def { iss = stringOrURI $ domainName conf
              , sub = stringOrURI me
              , iat = intDate $ utcTimeToPOSIXSeconds now }
      t' = encodeSigned HS256 (secret $ secretKey conf) t
  showForm [("access_token", t'), ("scope", "post"), ("me", me)]

doIndieAuth :: (?conf :: SweetrollConf, ?httpMgr :: Manager) => SweetrollAction () -> SweetrollAction ()
doIndieAuth onFail = do
  allParams <- params
  let par x = toStrict <$> findByKey allParams x
      par' = fromMaybe "" . par
      valid = makeAccessToken ?conf $ par' "me"
  if testMode ?conf then valid else do
    let reqBody = writeForm [ ("code",         par' "code")
                            , ("redirect_uri", par' "redirect_uri")
                            , ("client_id",    fromMaybe (baseUrl ?conf) $ par "client_id")
                            , ("state",        par' "state") ]
    indieAuthReq <- liftIO $ parseUrl $ indieAuthEndpoint ?conf
    resp <- liftIO $ request (indieAuthReq { method = "POST"
                                           , requestBody = RequestBodyBS reqBody }) :: SweetrollAction (Response LByteString)
    if responseStatus resp /= ok200 then onFail
    else valid
