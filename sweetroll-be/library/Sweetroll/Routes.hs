{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators, DataKinds #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           Sweetroll.Prelude
--import qualified Network.HTTP.Link as L
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response
import           Sweetroll.Auth (AccessToken)

type Auth = AuthProtect "jwt"

type PostLoginRoute           = "login" :> Host :> Form :> Post '[FormUrlEncoded, JSON] AccessToken
type GetLoginRoute            = "login" :> Auth :> Get '[FormUrlEncoded] [(Text, Text)]
type GetSelfLoginRoute        = "login" :> "self" :> Host :> QueryParam "me" Text :> QueryParam "code" Text :> QueryParam "scope" Text :> Get '[FormUrlEncoded] NoContent
type PostMicropubRoute        = "micropub" :> Auth :> Host :> ReqBody '[FormUrlEncoded, JSON] MicropubRequest :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type GetMicropubRoute         = "micropub" :> Auth :> Host :> QueryParam "q" Text :> QueryParams "properties" Text :> QueryParam "url" Text :> Get '[JSON, FormUrlEncoded] MicropubResponse
type PostWebmentionRoute      = "webmention" :> Form :> PostAccepted '[JSON] NoContent

type SweetrollAPI             = PostLoginRoute :<|> GetLoginRoute :<|> GetSelfLoginRoute
                           :<|> PostMicropubRoute :<|> GetMicropubRoute
                           :<|> PostWebmentionRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

-- XXX: From https://github.com/haskell-servant/servant/issues/133 -- delete when support lands in servant
data Tmp
data Files b
