{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeOperators #-}

module Sweetroll.App where

import           Sweetroll.Prelude hiding (Context)
import           Network.Wai
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Servant
import           Sweetroll.Conf
import           Sweetroll.Context
import           Sweetroll.Routes
import           Sweetroll.Auth
import           Sweetroll.Micropub.Endpoint
import           Sweetroll.Webmention.Receive

sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT = postLogin :<|> getAuth
                  :<|> postMedia :<|> postMicropub :<|> getMicropub
                  :<|> receiveWebmention

sweetrollApp ∷ SweetrollCtx → Application
sweetrollApp ctx =
    simpleCors
  $ autohead
  $ acceptOverride
  $ supportFormAuth
  $ serveWithContext sweetrollAPI sweetrollContext $ sweetrollServer ctx
  where sweetrollServer c = enter (sweetrollToExcept c) sweetrollServerT
        sweetrollContext = authHandler (secretKey $ getter ctx) :. EmptyContext

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = sweetrollApp <$> initCtx conf secs
