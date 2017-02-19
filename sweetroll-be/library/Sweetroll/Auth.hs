{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, DataKinds #-}

-- | The IndieAuth/rel-me-auth implementation, using JSON Web Tokens.
module Sweetroll.Auth (
  JWT
, VerifiedJWT
, AuthProtect
, AuthHandler
, module Sweetroll.Auth
) where

import           Sweetroll.Prelude hiding (iat, au)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import           Web.JWT hiding (header)
import qualified Network.Wai as Wai
import           Servant.API
import           Servant.Server.Experimental.Auth
import           Sweetroll.Monads
import           Sweetroll.Conf
import           Sweetroll.HTTPClient hiding (Header)

type instance AuthServerData (AuthProtect "jwt") = JWT VerifiedJWT

instance HasLink sub ⇒ HasLink (AuthProtect "jwt" :> sub) where
  type MkLink (AuthProtect "jwt" :> sub) = MkLink sub
  toLink _ = toLink (Proxy ∷ Proxy sub)

authHandler ∷ Text → AuthHandler Wai.Request (JWT VerifiedJWT)
authHandler secKey = mkAuthHandler h
  where h req =
          case asum [ lookup hAuthorization (Wai.requestHeaders req) >>= fromHeader
                    , join $ lookup "access_token" $ Wai.queryString req
                    ] of
            Nothing → throwE errNoAuth
            Just tok →
              case decodeAndVerifySignature (secret secKey) (cs tok) of
                Nothing → throwE errWrongAuth
                Just decodedToken → return decodedToken

fromHeader ∷ ByteString → Maybe ByteString
fromHeader "" = Nothing
fromHeader au = return $ drop 7 au -- 7 chars in "Bearer "

getAuth ∷ JWT VerifiedJWT → Sweetroll [(Text, Text)]
getAuth token = return $ ("me", maybe "" tshow $ sub $ claims token) : unreg
  where unreg = mapMaybe unValue $ M.toList $ unregisteredClaims $ claims token
        unValue (k, String s) = Just (k, s)
        unValue _ = Nothing

signAccessToken ∷ Text → Text → Text → UTCTime → Text → Text → Text
signAccessToken sec domain me now scope clientId = encodeSigned HS256 (secret sec) t
  where t = def { iss = stringOrURI domain
                , sub = stringOrURI me
                , iat = numericDate $ utcTimeToPOSIXSeconds now
                , unregisteredClaims = M.fromList [ ("scope", String scope)
                                                  , ("client_id", String clientId) ] }

makeAccessToken ∷ Text → Text → Text → Sweetroll [(Text, Text)]
makeAccessToken me scope clientId = do
  domain ← getConfOpt domainName
  secs ← getSecs
  now ← liftIO getCurrentTime
  return [ ("access_token", signAccessToken (secretKey secs) domain me now scope clientId)
         , ("scope", scope)
         , ("client_id", clientId)
         , ("me", me) ]

postLogin ∷ [(Text, Text)] → Sweetroll [(Text, Text)]
postLogin params = do
  isTestMode ← getConfOpt testMode
  if isTestMode
     then makeAccessToken (fromMaybe "unknown" $ lookup "me" params) "post" "example.com"
     else do
       checkURI ← getConfOpt indieAuthCheckEndpoint
       resp0 ← runHTTP $ reqS checkURI >>= postForm params >>= performWithBytes
       case (note "Could not read form" . readForm) =<< responseBody <$> resp0 of
         Right (indieAuthRespParams ∷ [(Text, Text)]) → do
           domain ← getConfOpt domainName
           let me = orEmptyMaybe $ lookup "me" indieAuthRespParams
           guardBool errWrongAuth $ Just domain == fmap (cs . uriRegName) (uriAuthority $ fromMaybe nullURI $ parseURI $ cs me)
           putStrLn $ cs $ "Authenticated a client: " ++ fromMaybe "unknown" (lookup "client_id" params)
           makeAccessToken me
                           (fromMaybe "post" $ lookup "scope" indieAuthRespParams)
                           (fromMaybe "example.com" $ lookup "client_id" params)
         Left e → do
           putStrLn $ cs $ "Authentication error: " ++ e ++ " / params: " ++ tshow params
           throwError errWrongAuth


supportFormAuth ∷ Wai.Middleware
supportFormAuth app req respond = do
  let headers = Wai.requestHeaders req
  if "urlencoded" `isInfixOf` (fromMaybe "" $ lookup hContentType headers)
     && not ("Bearer" `isInfixOf` (fromMaybe "" $ lookup hAuthorization headers))
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
          let rbody = atomicModifyIORef ichunks $ \chunks →
                 case chunks of
                     [] → ([], S8.empty)
                     x:y → (y, x)
          let rq' = rq { Wai.requestBody = rbody }
          return (rq', body)
