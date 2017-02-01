{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           ClassyPrelude
import qualified Network.HTTP.Link as L
import           Servant
import           Data.String.Conversions
import           Sweetroll.Micropub.Request
import           Sweetroll.Micropub.Response

type WithLink α               = (Headers '[Header "Link" [L.Link]] α)

type PostLoginRoute           = "login" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] [(Text, Text)]
type GetLoginRoute            = "login" :> AuthProtect "jwt" :> Get '[FormUrlEncoded] [(Text, Text)]
type PostMediaRoute           = "micropub" :> "media" :> AuthProtect "jwt" :> Files Tmp :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type PostMicropubRoute        = "micropub" :> AuthProtect "jwt" :> ReqBody '[FormUrlEncoded, JSON] MicropubRequest :> PostCreated '[FormUrlEncoded, JSON] (Headers '[Header "Location" Text] MicropubResponse)
type GetMicropubRoute         = "micropub" :> AuthProtect "jwt" :> QueryParam "q" Text :> QueryParams "properties" Text :> QueryParam "url" Text :> Get '[JSON, FormUrlEncoded] MicropubResponse

type PostWebmentionRoute      = "webmention" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> PostAccepted '[JSON] NoContent

type SweetrollAPI             = PostLoginRoute :<|> GetLoginRoute
                           :<|> PostMediaRoute :<|> PostMicropubRoute :<|> GetMicropubRoute
                           :<|> PostWebmentionRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

permalink ∷ (IsElem α SweetrollAPI, HasLink α) ⇒ Proxy α → MkLink α -- MkLink is URI
permalink = safeLink sweetrollAPI

showLink ∷ URI → Text
showLink = ("/" ++) . cs . show

-- XXX: From https://github.com/haskell-servant/servant/issues/133 -- delete when support lands in servant
data Tmp
data Files b
