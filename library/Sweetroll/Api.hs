{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           Sweetroll.Prelude hiding (Context)
import           Network.Wai
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Servant
import           Sweetroll.Conf
import           Sweetroll.Monads
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
  $ gzip def
  $ supportFormAuth
  $ (serveWithContext sweetrollAPI sweetrollContext $ sweetrollServer ctx)
  where sweetrollServer c = enter (sweetrollToExcept c) sweetrollServerT
        sweetrollContext = authHandler (secretKey $ _ctxSecs ctx) :. EmptyContext

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = fmap sweetrollApp $ initCtx conf secs
