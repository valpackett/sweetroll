{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

-- | The HTTP routes
module Sweetroll.Routes where

import           ClassyPrelude
import qualified Network.HTTP.Link as L
import           Servant hiding (toText)
import           Data.Stringable
import           Sweetroll.Auth (AuthProtect)
import           Sweetroll.Conf
import           Sweetroll.Pages

data HTML
data CSS

type WithLink α               = (Headers '[Header "Link" [L.Link]] α)

type IndieConfigRoute         = "indie-config" :> Get '[HTML] IndieConfig
type DefaultCssRoute          = "default-style.css" :> Get '[CSS] LByteString

type PostLoginRoute           = "login" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] [(Text, Text)]
type GetLoginRoute            = "login" :> AuthProtect :> Get '[FormUrlEncoded] [(Text, Text)]
type PostMicropubRoute        = "micropub" :> AuthProtect :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[FormUrlEncoded] (Headers '[Header "Location" Text] [(Text, Text)])
type GetMicropubRoute         = "micropub" :> AuthProtect :> QueryParam "q" Text :> Get '[FormUrlEncoded] [(Text, Text)]

type EntryRoute               = Capture "catName" String :> Capture "slug" String :> Get '[HTML] (WithLink (View EntryPage))
type CatRoute                 = Capture "catName" String :> QueryParam "before" Int :> QueryParam "after" Int :> Get '[HTML] (WithLink (View CatPage))
type CatRouteE                = Capture "catName" String :> Get '[HTML] (WithLink (View CatPage))
type CatRouteB                = Capture "catName" String :> QueryParam "before" Int :> Get '[HTML] (WithLink (View CatPage))
type CatRouteA                = Capture "catName" String :> QueryParam "after" Int :> Get '[HTML] (WithLink (View CatPage))
type IndexRoute               = Get '[HTML] (WithLink (View IndexPage))

type SweetrollAPI             = IndieConfigRoute :<|> DefaultCssRoute
                           :<|> PostLoginRoute :<|> GetLoginRoute
                           :<|> PostMicropubRoute :<|> GetMicropubRoute
                           :<|> EntryRoute :<|> CatRoute :<|> IndexRoute

sweetrollAPI ∷ Proxy SweetrollAPI
sweetrollAPI = Proxy

permalink ∷ (IsElem α SweetrollAPI, HasLink α) ⇒ Proxy α → MkLink α -- MkLink is URI
permalink = safeLink sweetrollAPI

showLink ∷ URI → Text
showLink = ("/" ++) . toText . show
