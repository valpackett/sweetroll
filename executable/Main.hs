{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           System.Environment
import           System.IO
import           Sweetroll.Conf
import           Sweetroll.Api (initSweetrollApp)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Crypto.Random
import           Crypto.Hash
import           Network.Wai.Cli

-- TODO run https://github.com/stackbuilders/dotenv-hs

main ∷ IO ()
main = do
  -- TODO domain and other stuff from env
  let conf = def
  envSecret ← lookupEnv "SWEETROLL_SECRET"
  secretVal ← case envSecret of
                  Just k | length k >= 40 → return $ hashWith SHA3_512 $ C8.pack k
                  _ → do
                    hPutStrLn stderr "Warning: the SWEETROLL_SECRET value is shorter than 40 characters. Not using it and generating a random one. Authentication tokens will expire after restarting Sweetroll."
                    randBytes ← getRandomBytes 64 ∷ IO BS.ByteString
                    return $ hashWith SHA3_512 randBytes
  let secs = def { secretKey = T.pack $ show secretVal }
  defWaiMain =<< initSweetrollApp conf secs
