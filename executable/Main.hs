{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP, UnicodeSyntax, TemplateHaskell, FlexibleContexts #-}

module Main where

import           Prelude
import           GHC.Conc (getNumCapabilities)
import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp
#ifdef SweetrollTLS
import           Network.Wai.Handler.WarpTLS
#endif
import           Network.Wai.Middleware.RequestLogger
import           Network.Socket.Activation
import           System.Posix.Internals (setNonBlockingFD)
import qualified Network.Socket as S
import           Control.Monad
import           Control.Exception
import           System.Console.ANSI
import           System.Directory
import           System.IO
import           Sweetroll.Conf
import           Sweetroll.Api (initSweetrollApp)
import qualified Data.Text as T
import           Data.Streaming.Network (bindPath)
import           Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteArray as BA
import           Crypto.Random
import           Crypto.Hash
import           Distribution.PackageDescription.TH
import           Git.Embed
import           Options
import           Gitson

data AppOptions = AppOptions
  { port                     ∷ Int
  , socket                   ∷ String
  , protocol                 ∷ String
  , tlsKeyFile               ∷ String
  , tlsCertFile              ∷ String
  , devlogging               ∷ Maybe Bool
  , domain                   ∷ Maybe String
  , secret                   ∷ String
  , https                    ∷ Maybe Bool
  , repo                     ∷ FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "port"              3000                                  "The port the app should listen for connections on (for http and http+tls protocols)"
    <*> simpleOption "socket"            "/var/run/sweetroll/sweetroll.sock"   "The UNIX domain socket the app should listen for connections on (for unix protocol)"
#ifdef SweetrollTLS
    <*> simpleOption "protocol"          "http"                                "The protocol for the server. One of: http, http+tls, unix, unix+tls, activate, activate+tls, cgi"
#else
    <*> simpleOption "protocol"          "http"                                "The protocol for the server. One of: http, unix, activate, cgi"
#endif
    <*> simpleOption "tlskey"            ""                                    "Path to the TLS private key file for +tls protocols"
    <*> simpleOption "tlscert"           ""                                    "Path to the TLS certificate bundle file for +tls protocols"
    <*> simpleOption "devlogging"        Nothing                               "Whether development logging should be enabled"
    <*> simpleOption "domain"            Nothing                               "The domain on which the server will run"
    <*> simpleOption "secret"            "RANDOM"                              "The JWT secret key for IndieAuth. Must be at least 40 characters, otherwise a random one will be used"
    <*> simpleOption "https"             Nothing                               "Whether HTTPS works on the domain"
    <*> simpleOption "repo"              "./"                                  "The git repository directory of the website"

setReset = setSGR [ Reset ]
boldYellow  x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow ] >> putStr x
boldMagenta x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta ] >> putStr x
red   x = setReset >> setSGR [ SetColor Foreground Dull Red ] >> putStr x
green x = setReset >> setSGR [ SetColor Foreground Dull Green ] >> putStr x
blue  x = setReset >> setSGR [ SetColor Foreground Dull Blue ] >> putStr x
reset x = setReset >> putStr x

putSweetroll = putStrLn "" >> putStr "   -=@@@ " >> green "Let me guess, someone stole your " >> boldYellow "sweetroll" >> green "?" >> setReset >> putStrLn " @@@=-"

runActivated cb = do
  ass ← getActivatedSockets
  case ass of
    Just ss → do
      _ ← forM ss $ \sock → do
        setNonBlockingFD (S.fdSocket sock) True
        cb sock
      return ()
    Nothing → putStrLn "No sockets to activate"

main ∷ IO ()
main = runCommand $ \opts _ → do
  setCurrentDirectory $ repo opts
  cpus ← getNumCapabilities
  let printProto = case protocol opts of
                     "http" → reset " port "   >> boldMagenta (show $ port opts)
                     "unix" → reset " socket " >> boldMagenta (show $ socket opts)
#ifdef SweetrollTLS
                     "http+tls" → reset " (TLS) port "   >> boldMagenta (show $ port opts)
                     "unix+tls" → reset " (TLS) socket " >> boldMagenta (show $ socket opts)
#endif
                     _      → setReset
      version = $(packageVariable $ pkgVersion . package) ++ "/" ++ $(embedGitShortRevision)
      printListening = boldYellow " Sweetroll " >> red version >> reset " running on " >> blue (protocol opts) >> printProto >> reset " with " >> green (show cpus ++ " CPUs") >> putStrLn ""
      warpSettings = setBeforeMainLoop printListening $ setPort (port opts) defaultSettings
#ifdef SweetrollTLS
      tlsSettings' = tlsSettings (tlsCertFile opts) (tlsKeyFile opts)
#endif

  origConf ← readDocument "conf" "sweetroll" ∷ IO (Maybe SweetrollConf)
  let conf = (fromMaybe def origConf) { httpsWorks = https opts , domainName = T.pack <$> domain opts }
  when (isNothing origConf) . transaction "." . saveDocument "conf" "sweetroll" $ conf

  secretVal ← case secret opts of
                k | length k >= 40 → return $ hashWith SHA3_512 $ C8.pack k
                _ → do
                  hPutStrLn stderr "Warning: the --secret value is shorter than 40 characters. Not using it and generating a random one. Authentication tokens will expire after restarting Sweetroll."
                  randBytes ← getRandomBytes 64 ∷ IO BS.ByteString
                  return $ hashWith SHA3_512 randBytes
  let secs = def { secretKey = T.pack $ show secretVal
                 , proxySigningKey = BA.pack $ BA.unpack secretVal }

  let addLogging = case devlogging opts of
                     Just True → logStdoutDev
                     _ → id

  app ← liftM addLogging $ initSweetrollApp conf secs
  case protocol opts of
    "http" → putSweetroll >> runSettings warpSettings app
    "unix" → putSweetroll >> bracket (bindPath $ socket opts) S.close (\sock → runSettingsSocket warpSettings sock app)
    "activate" → putSweetroll >> runActivated (\sock → runSettingsSocket warpSettings sock app)
#ifdef SweetrollTLS
    "http+tls" → putSweetroll >> runTLS tlsSettings' warpSettings app
    "unix+tls" → putSweetroll >> bracket (bindPath $ socket opts) S.close (\sock → runTLSSocket tlsSettings' warpSettings sock app)
    "activate+tls" → putSweetroll >> runActivated (\sock → runTLSSocket tlsSettings' warpSettings sock app)
#endif
    "cgi" → CGI.run app
    _ → putStrLn $ "Unsupported protocol: " ++ protocol opts
