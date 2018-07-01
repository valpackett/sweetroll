{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DataKinds, QuasiQuotes, TemplateHaskell, DeriveGeneric, LambdaCase #-}

-- | The IndieAuth/rel-me-auth implementation, using JSON Web Tokens.
module Sweetroll.Auth (
  JWT
, VerifiedJWT
, AuthProtect
, module Sweetroll.Auth
) where

import           Sweetroll.Prelude as SP hiding (iat, au, host)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Maybe (fromJust)
import           Data.Text (dropWhileEnd)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import           Web.JWT hiding (Header)
import qualified Network.Wai as Wai
import           Servant.Server.Experimental.Auth
import           Servant.Server
import           Web.Cookie (parseCookies)
import           Web.FormUrlEncoded as FU
import           Sweetroll.Context
import           Sweetroll.Conf

type instance AuthServerData (AuthProtect "jwt") = JWT VerifiedJWT

instance HasLink sub ⇒ HasLink (AuthProtect "jwt" :> sub) where
  type MkLink (AuthProtect "jwt" :> sub) = MkLink sub
  toLink _ = toLink (Proxy ∷ Proxy sub)

data AccessToken = AccessToken
  { accessToken ∷ Text
  , scope ∷ Text
  , clientId ∷ Text
  , me ∷ Text } deriving (Generic)

instance ToJSON AccessToken where
  toEncoding = genericToEncoding defaultOptions { SP.fieldLabelModifier = camelTo2 '_' }

instance ToForm AccessToken where
  toForm = genericToForm FormOptions { FU.fieldLabelModifier = camelTo2 '_' }

authHandler ∷ Text → AuthHandler Wai.Request (JWT VerifiedJWT)
authHandler secKey = mkAuthHandler h
  where h req = Servant.Server.Handler $
          case asum [ fromHeader =<< lookup hAuthorization (Wai.requestHeaders req)
                    , join $ lookup "access_token" $ Wai.queryString req
                    , lookup "Bearer" =<< parseCookies <$> lookup hCookie (Wai.requestHeaders req)
                    ] of
            Nothing → throwM errNoAuth
            Just tok →
              case decodeAndVerifySignature (secret secKey) (cs tok) of
                Nothing → throwM errWrongAuth
                Just decodedToken → do
                  -- !!! Only allow tokens issued by the current Host !!!
                  if (fromMaybe "" $ lookup "Host" $ Wai.requestHeaders req) == (cs $ show $ fromJust $ iss $ claims decodedToken)
                  then return decodedToken
                  else throwM errWrongAuth

fromHeader ∷ ByteString → Maybe ByteString
fromHeader "" = Nothing
fromHeader au = return $ drop 7 au -- 7 chars in "Bearer "

getAuth ∷ JWT VerifiedJWT → Sweetroll [(Text, Text)]
getAuth token = return $ ("me", maybe "" tshow $ sub $ claims token) : unregClaims token

unregClaims ∷ JWT VerifiedJWT → [(Text, Text)]
unregClaims = mapMaybe unValue . M.toList . unregisteredClaims . claims
  where unValue (k, String s) = Just (k, s)
        unValue _ = Nothing

signAccessToken ∷ Text → Text → Text → UTCTime → Text → Text → Text
signAccessToken sec domain me now scope clientId = encodeSigned HS256 (secret sec) t
  where t = def { iss = stringOrURI domain
                , sub = stringOrURI $ dropWhileEnd (== '/') me
                , iat = numericDate $ utcTimeToPOSIXSeconds now
                , unregisteredClaims = M.fromList [ ("scope", String scope)
                                                  , ("client_id", String clientId) ] }

makeAccessToken ∷ Text → Text → Text → Text → Sweetroll AccessToken
makeAccessToken domain me scope clientId = do
  secs ← getSecs
  now ← liftIO getCurrentTime
  return $ AccessToken { accessToken = signAccessToken (secretKey secs) domain me now scope clientId
                       , scope = scope
                       , clientId = clientId
                       , me = me }

postLogin ∷ Maybe Text → [(Text, Text)] → Sweetroll AccessToken
postLogin host params = do
  let domain = fromMaybe "localhost" host
  isTestMode ← getConfOpt testMode
  --let isTestMode = False
  if isTestMode
     then makeAccessToken domain (fromMaybe "unknown" $ lookup "me" params) (fromMaybe "post" $ lookup "scope" params) "example.com"
     else do
       checkURI ← getConfOpt indieauthCheckEndpoint
       resp0 ← runHTTP $ reqS checkURI >>= postForm params >>= performWithBytes
       case (note "Could not read form" . readForm) =<< responseBody <$> resp0 of
         Right (indieAuthRespParams ∷ [(Text, Text)]) → do
           let me = orEmptyMaybe $ lookup "me" indieAuthRespParams
           guardBool errWrongAuth $ Just domain == fmap (cs . uriRegName) (uriAuthority $ fromMaybe nullURI $ parseURI $ cs me)
           logWarn $ "Authenticated a client: " ++ display (fromMaybe "unknown" $ lookup "client_id" params)
           makeAccessToken domain me
                           (fromMaybe "post" $ lookup "scope" indieAuthRespParams)
                           (fromMaybe "" $ lookup "client_id" params)
         Left e → do
           logInfo $ "Authentication error: " ++ display e ++ " / params: " ++ display (tshow params)
           throwM errWrongAuth

getSelfLogin ∷ Maybe Text → Maybe Text → Maybe Text → Maybe Text → Sweetroll NoContent
getSelfLogin host me code scope = do
  let domain = fromMaybe "localhost" host
  isTestMode ← getConfOpt testMode
  --let isTestMode = False
  isHttpOnly ← not <$> getConfOpt allowJsCookieAccess
  let security = if isTestMode then "" else "; Secure; SameSite=Strict"
        ++ if isHttpOnly then "; HttpOnly" else ""
  result ← postLogin host [ ("me", fromMaybe ("https://" ++ domain) me)
                          , ("code", fromMaybe "" code)
                          , ("scope", fromMaybe "post" scope)
                          , ("redirect_uri", "https://" ++ domain ++ "/login/self")
                          , ("client_id", "https://" ++ domain ++ "/")
                          , ("grant_type", "authorization_code") ]
  throwM err303 { errHeaders = [ ("Set-Cookie", "Bearer=" ++ (cs $ accessToken result) ++ "; Path=/; Max-Age=5184000" ++ security)
                                   , (hLocation, "/") ] }

errNoAuth ∷ ServantErr
errNoAuth = errText err401 "Authorization/access_token not found."

errWrongAuth ∷ ServantErr
errWrongAuth = errText err401 "Invalid auth token."

errWrongAuthScope ∷ ServantErr
errWrongAuthScope = errText err401 "Your access token is not authorized for this action."

ensureScope ∷ MonadThrow μ ⇒ JWT VerifiedJWT → ([Text] → Bool) → μ ()
ensureScope token p = guardBool errWrongAuthScope $ p scopes
  where scopes = words $ fromMaybe "" $ lookup "scope" $ unregClaims token

supportFormAuth ∷ Wai.Middleware
supportFormAuth app req respond = do
  let headers = Wai.requestHeaders req
  if "urlencoded" `isInfixOf` (fromMaybe "" $ lookup hContentType headers)
     && not ("Bearer" `isInfixOf` (fromMaybe "" $ lookup hAuthorization headers))
     && not ("Bearer" `isInfixOf` (fromMaybe "" $ lookup hCookie headers))
     then do
       (req', body) ← getRequestBody req
       let form = fromMaybe [] $ readForm $ S8.concat body ∷ [(ByteString, ByteString)]
       let token = fromMaybe "" $ lookup "access_token" form
       app req' { Wai.requestHeaders = (hAuthorization, "Bearer " ++ token) : headers } respond
     else app req respond
  where getRequestBody rq = do
          -- https://hackage.haskell.org/package/wai-extra-3.0.16.1/docs/src/Network.Wai.Middleware.rquestLogger.html
          let loop front = do
                 bs ← Wai.requestBody rq
                 if S8.null bs
                     then return $ front []
                     else loop $ front . (bs:)
          body ← loop id
          ichunks ← newIORef body
          let rbody = atomicModifyIORef ichunks $ \case
                     [] → ([], S8.empty)
                     x:y → (y, x)
          let rq' = rq { Wai.requestBody = rbody }
          return (rq', body)
