{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

-- | https://github.com/chrisdone/ghci-reload-demo/blob/master/src/DevelMain.hs
module DevMain where

import           Sweetroll.Prelude
import           Control.Concurrent (forkIO)
import           System.IO.Unsafe
import           Foreign.Store
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Gitson.Util (insideDirectory)
import           Sweetroll.Api
import           Sweetroll.Conf

-- | Basically, a lock on the current working directory...
--   Which means there won't be any concurrency, but that's fine in dev.
--   Just don't benchmark it when it's running in GHCi!
sem ∷ MVar ()
sem = unsafePerformIO $ newMVar ()

app ∷ IO Application
app = withMVar sem $ \_ → insideDirectory "/tmp/sroll" $ liftM logStdoutDev $ initSweetrollApp def { testMode = Just True } def { secretKey = "TESTKEY" }

main ∷ Int → IO ()
main port = do
  ref ← newIORef =<< app
  _tid ← forkIO $ do
    runSettings (setPort port defaultSettings)
                (\r req → do
                  handler ← readIORef ref
                  withMVar sem $ \_ → insideDirectory "/tmp/sroll" (handler r req))
  _ ← newStore ref
  return ()

-- XXX: move to higher level library https://hackage.haskell.org/package/rapid

update ∷ Int → IO ()
update port = do
  m ← lookupStore 0
  case m of
    Nothing → main port
    Just store → do
      ref ← readStore store
      writeIORef ref =<< app
