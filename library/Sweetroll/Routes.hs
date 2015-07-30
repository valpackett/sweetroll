{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           ClassyPrelude
import qualified Network.HTTP.Link as L
import           Servant
import           Sweetroll.Auth (AuthProtect)
import           Sweetroll.Conf
import           Sweetroll.Pages

data HTML
data CSS

type WithLink α               = (Headers '[Header "Link" [L.Link]] α)

type LoginRoute               = "login" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] [(Text, Text)]
type IndieConfigRoute         = "indie-config" :> Get '[HTML] IndieConfig
type DefaultCssRoute          = "default-style.css" :> Get '[CSS] LByteString
type EntryRoute               = Capture "catName" String :> Capture "slug" String :> Get '[HTML] (WithLink (View EntryPage))
type CatRoute                 = Capture "catName" String :> QueryParam "page" Int :> Get '[HTML] (WithLink (View CatPage))
type IndexRoute               = Get '[HTML] (WithLink (View IndexPage))

type PostMicropubRoute        = "micropub" :> AuthProtect :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] (Headers '[Header "Location" Text] [(Text, Text)])
type GetMicropubRoute         = "micropub" :> AuthProtect :> QueryParam "q" Text :> Get '[FormUrlEncoded] [(Text, Text)]

type SweetrollAPI             = LoginRoute :<|> IndieConfigRoute :<|> DefaultCssRoute
                           :<|> EntryRoute :<|> CatRoute :<|> IndexRoute
                           :<|> PostMicropubRoute :<|> GetMicropubRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy
