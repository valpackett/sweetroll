{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

-- | The IndieAuth/rel-me-auth implementation, using JSON Web Tokens.
module Sweetroll.Auth (
  JWT
, VerifiedJWT
, module Sweetroll.Auth
) where

import           Sweetroll.Prelude hiding (iat, au)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Map as M
import           Web.JWT hiding (header)
import qualified Network.Wai as Wai
import           Servant
import           Servant.Server.Internal (succeedWith)
import           Servant.Server.Internal.Enter
import           Sweetroll.Monads
import           Sweetroll.Conf
import           Sweetroll.HTTPClient hiding (Header)

data AuthProtect
data AuthProtected α = AuthProtected Text α

instance Enter α φ β ⇒ Enter (AuthProtected α) φ (AuthProtected β) where
  enter f (AuthProtected k a) = AuthProtected k $ enter f a

instance HasServer sublayout ⇒ HasServer (AuthProtect :> sublayout) where
  type ServerT (AuthProtect :> sublayout) α = AuthProtected (JWT VerifiedJWT → ServerT sublayout α)

  route Proxy (AuthProtected secKey subserver) req respond =
    case asum [ lookup hAuthorization (Wai.requestHeaders req) >>= fromHeader
              , join $ lookup "access_token" $ Wai.queryString req ] of
      Nothing → respond . succeedWith $ Wai.responseLBS status401 [] "Authorization/access_token not found."
      Just tok →
        case decodeAndVerifySignature (secret secKey) (cs tok) of
          Nothing → respond . succeedWith $ Wai.responseLBS status401 [] "Invalid auth token."
          Just decodedToken → route (Proxy ∷ Proxy sublayout) (subserver decodedToken) req respond

instance HasLink sub ⇒ HasLink (AuthProtect :> sub) where
  type MkLink (AuthProtect :> sub) = MkLink sub
  toLink _ = toLink (Proxy ∷ Proxy sub)

fromHeader ∷ ByteString → Maybe ByteString
fromHeader "" = Nothing
fromHeader au = return $ drop 7 au -- 7 chars in "Bearer "

getAuth ∷ JWT VerifiedJWT → Sweetroll [(Text, Text)]
getAuth token = return $ [ ("me", maybe "" tshow $ sub $ claims token) ] ++ unreg
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
           guardBool err401 $ Just domain == (fmap (cs . uriRegName) $ uriAuthority $ fromMaybe nullURI $ parseURI $ cs me)
           putStrLn $ cs $ "Authenticated a client: " ++ fromMaybe "unknown" (lookup "client_id" params)
           makeAccessToken me
                           (fromMaybe "post" $ lookup "scope" indieAuthRespParams)
                           (fromMaybe "example.com" $ lookup "client_id" params)
         Left e → do
           putStrLn $ cs $ "Authentication error: " ++ e ++ " / params: " ++ tshow params
           throwError err401
