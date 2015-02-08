{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp
import qualified Network.Socket as S
import           System.Console.ANSI
import           Control.Applicative
import           Control.Exception
import           System.Directory
import           System.Entropy
import           Sweetroll.Conf
import qualified Sweetroll.App as A
import qualified Sweetroll.Monads as M
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Streaming.Network (bindPath)
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Hash.RIPEMD160 as H
import           Options

data AppOptions = AppOptions
  { port                     :: Int
  , socket                   :: String
  , protocol                 :: String
  , domain                   :: String
  , sitename                 :: String
  , itemsperpage             :: Int
  , indieauthc               :: String
  , indieauthr               :: String
  , adnhost                  :: String
  , adntoken                 :: String
  , twihost                  :: String
  , twiappkey                :: String
  , twiappsecret             :: String
  , twiacctoken              :: String
  , twiaccsecret             :: String
  , secret                   :: String
  , https                    :: Bool
  , repo                     :: FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "port"              3000                                  "The port the app should listen for connections on (for http protocol)"
    <*> simpleOption "socket"            "/var/run/sweetroll/sweetroll.sock"   "The UNIX domain socket the app should listen for connections on (for unix protocol)"
    <*> simpleOption "protocol"          "http"                                "The protocol for the server. One of: http, unix, cgi"
    <*> simpleOption "domain"            "localhost:3000"                      "The domain on which the server will run"
    <*> simpleOption "sitename"          "A new Sweetroll website"             "The name of the website"
    <*> simpleOption "itemsperpage"      20                                    "The number of items displayed on each page"
    <*> simpleOption "indieauthc"        "https://indieauth.com/auth"          "The IndieAuth check endpoint to use"
    <*> simpleOption "indieauthr"        "https://indieauth.com/auth"          "The IndieAuth redirect endpoint to use"
    <*> simpleOption "adnhost"           "https://api.app.net"                 "The App.net API host to use"
    <*> simpleOption "adntoken"          ""                                    "The App.net API access token to use"
    <*> simpleOption "twihost"           "https://api.twitter.com/1.1"         "The Twitter API 1.1 host to use"
    <*> simpleOption "twiappkey"         ""                                    "The Twitter API application key to use"
    <*> simpleOption "twiappsecret"      ""                                    "The Twitter API application token secret to use"
    <*> simpleOption "twiacctoken"       ""                                    "The Twitter API access token to use"
    <*> simpleOption "twiaccsecret"      ""                                    "The Twitter API access token secret to use"
    <*> simpleOption "secret"            "RANDOM"                              "The JWT secret key for IndieAuth"
    <*> simpleOption "https"             False                                 "Whether HTTPS works on the domain"
    <*> simpleOption "repo"              "./"                                  "The git repository directory of the website"

setReset = setSGR [ Reset ]
boldYellow  x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow ] >> putStr x
boldMagenta x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta ] >> putStr x
red   x = setReset >> setSGR [ SetColor Foreground Dull Red ] >> putStr x
green x = setReset >> setSGR [ SetColor Foreground Dull Green ] >> putStr x
blue  x = setReset >> setSGR [ SetColor Foreground Dull Blue ] >> putStr x
reset x = setReset >> putStr x

putSweetroll = putStrLn "" >> putStr "  -=@@@ " >> green "Let me guess, someone stole your " >> boldYellow "sweetroll" >> green "?" >> setReset >> putStrLn " @@@=-"

main :: IO ()
main = runCommand $ \opts args -> do
  setCurrentDirectory (repo opts)
  let printProto = case protocol opts of
                     "http" -> reset " port "   >> boldMagenta (show $ port opts)
                     "unix" -> reset " socket " >> boldMagenta (show $ socket opts)
                     _      -> setReset
      printListening = boldYellow "         Sweetroll " >> red "0.0.0" >> reset " running on " >> blue (protocol opts) >> printProto >> setReset >> putStrLn ""
      warpSettings = setBeforeMainLoop printListening $ setPort (port opts) defaultSettings
  randBytes <- getEntropy 64
  let secret' = case secret opts of
                  "RANDOM" -> decodeUtf8 $ B64.encode $ H.hash randBytes
                  k -> T.pack k
  conf <- loadTemplates def {
    domainName                     = T.pack $ domain opts
  , secretKey                      = secret'
  , httpsWorks                     = https opts
  , siteName                       = T.pack $ sitename opts
  , itemsPerPage                   = itemsperpage opts
  , indieAuthCheckEndpoint         = indieauthc opts
  , indieAuthRedirEndpoint         = indieauthr opts
  , adnApiHost                     = adnhost opts
  , adnApiToken                    = adntoken opts
  , twitterApiHost                 = twihost opts
  , twitterAppKey                  = encodeUtf8 $ T.pack $ twiappkey opts
  , twitterAppSecret               = encodeUtf8 $ T.pack $ twiappsecret opts
  , twitterAccessToken             = encodeUtf8 $ T.pack $ twiacctoken opts
  , twitterAccessSecret            = encodeUtf8 $ T.pack $ twiaccsecret opts
  }
  let app = M.sweetrollApp conf A.app
  case protocol opts of
    "http" -> putSweetroll >> app >>= runSettings warpSettings
    "unix" -> putSweetroll >>
      bracket (bindPath $ socket opts)
              S.close
              (\socket -> app >>= runSettingsSocket warpSettings socket)
    "cgi" -> app >>= CGI.run
    _ -> putStrLn $ "Unsupported protocol: " ++ protocol opts
