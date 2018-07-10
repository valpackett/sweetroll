{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators, DataKinds #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           Sweetroll.Prelude
--import qualified Network.HTTP.Link as L
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response
import           Sweetroll.Microsub.Request
import           Sweetroll.Microsub.Response
import           Sweetroll.Auth (AccessToken)

type Auth = AuthProtect "jwt"

type PostLoginRoute           = "login" :> Host :> Form :> Post '[FormUrlEncoded, JSON] AccessToken
type GetLoginRoute            = "login" :> Auth :> Get '[FormUrlEncoded] [(Text, Text)]
type GetSelfLoginRoute        = "login" :> "self" :> Host :> QueryParam "me" Text :> QueryParam "code" Text :> QueryParam "scope" Text :> Get '[FormUrlEncoded] NoContent
type GetTestLoginRoute        = "login" :> "test" :> Host :> QueryParam "state" Text :> QueryParam "redirect_uri" Text :> Get '[FormUrlEncoded] NoContent
type PostMicropubRoute        = "micropub" :> Auth :> Host :> ReqBody '[FormUrlEncoded, JSON] MicropubRequest :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type GetMicropubRoute         = "micropub" :> Auth :> Host :> QueryParam "q" Text :> QueryParams "properties" Text :> QueryParam "url" Text :> Get '[JSON, FormUrlEncoded] MicropubResponse
type PostMicrosubRoute        = "microsub" :> Auth :> Host :> ReqBody '[FormUrlEncoded] MicrosubRequest :> Post '[JSON] MicrosubResponse
type GetMicrosubRoute         = "microsub" :> Auth :> Host :> QueryParam "action" Text :> QueryParam "channel" Text :> QueryParam "after" Text :> QueryParam "before" Text :> Get '[JSON] MicrosubResponse
type PostPollFeedsRoute       = "microsub" :> "poll" :> QueryParam "token" Text :> Post '[JSON] NoContent
type PostWebmentionRoute      = "webmention" :> Form :> PostAccepted '[JSON] NoContent

type SweetrollAPI             = PostLoginRoute :<|> GetLoginRoute :<|> GetSelfLoginRoute :<|> GetTestLoginRoute
                           :<|> PostMicropubRoute :<|> GetMicropubRoute
                           :<|> PostMicrosubRoute :<|> GetMicrosubRoute
                           :<|> PostPollFeedsRoute
                           :<|> PostWebmentionRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

-- XXX: From https://github.com/haskell-servant/servant/issues/133 -- delete when support lands in servant
data Tmp
data Files b
