{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators, DataKinds #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           ClassyPrelude
--import qualified Network.HTTP.Link as L
import           Servant
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response

type Auth = AuthProtect "jwt"
type Host = Header "Host" Text
type Form = ReqBody '[FormUrlEncoded] [(Text, Text)]
-- type WithLink α = (Headers '[Header "Link" [L.Link]] α)

type PostLoginRoute           = "login" :> Host :> Form :> Post '[FormUrlEncoded] [(Text, Text)]
type GetLoginRoute            = "login" :> Auth :> Get '[FormUrlEncoded] [(Text, Text)]
type GetSelfLoginRoute        = "login" :> "self" :> Host :> QueryParam "me" Text :> QueryParam "code" Text :> Get '[FormUrlEncoded] NoContent
type PostMediaRoute           = "micropub" :> "media" :> Auth :> Host :> Files Tmp :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type PostMicropubRoute        = "micropub" :> Auth :> Host :> ReqBody '[FormUrlEncoded, JSON] MicropubRequest :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type GetMicropubRoute         = "micropub" :> Auth :> Host :> QueryParam "q" Text :> QueryParams "properties" Text :> QueryParam "url" Text :> Get '[JSON, FormUrlEncoded] MicropubResponse
type PostWebmentionRoute      = "webmention" :> Form :> PostAccepted '[JSON] NoContent

type SweetrollAPI             = PostLoginRoute :<|> GetLoginRoute :<|> GetSelfLoginRoute
                           :<|> PostMediaRoute :<|> PostMicropubRoute :<|> GetMicropubRoute
                           :<|> PostWebmentionRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

-- XXX: From https://github.com/haskell-servant/servant/issues/133 -- delete when support lands in servant
data Tmp
data Files b
