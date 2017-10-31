{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators #-}

module Sweetroll.App where

import           Sweetroll.Prelude hiding (Context)
import           Servant.Server
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Sweetroll.Conf
import           Sweetroll.Context
import           Sweetroll.Routes
import           Sweetroll.Auth
import           Sweetroll.Micropub.Endpoint
import           Sweetroll.Webmention.Receive

sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT = postLogin :<|> getAuth :<|> getSelfLogin
                  :<|> postMicropub :<|> getMicropub
                  :<|> receiveWebmention

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx =
    simpleCors
  $ autohead
  $ acceptOverride
  $ supportFormAuth
  $ magicbaneApp sweetrollAPI sweetrollContext ctx sweetrollServerT
  where sweetrollContext = authHandler (secretKey $ getter ctx) :. EmptyContext

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = sweetrollApp <$> initCtx conf secs
