{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Sweetroll.Api where

import           Sweetroll.Prelude hiding (Context)
import           Network.Wai
import           Network.Wai.UrlMap
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Throttle
import           Servant
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Auth
import           Sweetroll.Micropub.Endpoint
import           Sweetroll.Webmention.Receive
import           Sweetroll.Proxy

sweetrollServerT ∷ ServerT SweetrollAPI Sweetroll
sweetrollServerT = postLogin :<|> getAuth
                  :<|> postMedia :<|> postMicropub :<|> getMicropub
                  :<|> receiveWebmention

sweetrollApp ∷ WaiThrottle → SweetrollCtx → Application
sweetrollApp thr ctx =
    simpleCors
  $ throttle defThr { isThrottled = (\r → return $ requestMethod r /= "GET" && requestMethod r /= "HEAD")
                    , throttleBurst = 5 } thr
  $ autohead
  $ acceptOverride
  $ gzip def
  $ supportFormAuth
  $ mapUrls $ mount "proxy" (requestProxy ctx)
          <|> mountRoot (serveWithContext sweetrollAPI sweetrollContext $ sweetrollServer ctx)
  where sweetrollServer c = enter (sweetrollToExcept c) sweetrollServerT
        sweetrollContext = authHandler (secretKey $ _ctxSecs ctx) :. EmptyContext
        defThr = defaultThrottleSettings

initSweetrollApp ∷ SweetrollConf → SweetrollSecrets → IO Application
initSweetrollApp conf secs = do
  thr ← initThrottler
  fmap (sweetrollApp thr) $ initCtx conf secs
