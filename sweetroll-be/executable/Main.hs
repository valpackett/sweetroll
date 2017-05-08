{-# LANGUAGE UnicodeSyntax, OverloadedStrings, LambdaCase #-}

module Main where

import           Prelude
import           System.IO
import           System.Envy
import           System.Environment
import           Sweetroll.Conf
import           Sweetroll.App (initSweetrollApp)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Crypto.Random
import           Crypto.Hash
import           Network.Wai.Cli
import           Debug.Trace

main ∷ IO ()
main = do
  conf ← decodeEnv >>= \case
                            Left e → hPutStrLn stderr ("Warning: error while reading env vars: " ++ e) >> return def
                            Right c → return c
  traceShowM conf
  envSecret ← lookupEnv "SWEETROLL_SECRET"
  secretVal ← case envSecret of
                  Just k | length k >= 40 → return $ T.pack k
                  _ → do
                    hPutStrLn stderr "Warning: the SWEETROLL_SECRET value is shorter than 40 characters. Not using it and generating a random one. Authentication tokens will expire after restarting Sweetroll."
                    randBytes ← getRandomBytes 64 ∷ IO BS.ByteString
                    return $ T.pack $ show $ hashWith SHA3_512 randBytes
  let secs = def { secretKey = secretVal }
  defWaiMain =<< initSweetrollApp conf secs
