{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE UnicodeSyntax, TemplateHaskell #-}

module Main (main) where

import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp
import qualified Network.Socket as S
import           System.Console.ANSI
import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           System.Directory
import           System.Entropy
import           Sweetroll.Conf
import qualified Sweetroll.App as A
import qualified Sweetroll.Monads as M
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Streaming.Network (bindPath)
import           Data.Maybe
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Hash.RIPEMD160 as H
import           Distribution.PackageDescription.TH
import           Git.Embed
import           Options
import           Gitson

data AppOptions = AppOptions
  { port                     ∷ Int
  , socket                   ∷ String
  , protocol                 ∷ String
  , domain                   ∷ Maybe String
  , indieauthc               ∷ Maybe String
  , indieauthr               ∷ Maybe String
  , pushhub                  ∷ Maybe String
  , adnhost                  ∷ Maybe String
  , adntoken                 ∷ String
  , twihost                  ∷ Maybe String
  , twiappkey                ∷ String
  , twiappsecret             ∷ String
  , twiacctoken              ∷ String
  , twiaccsecret             ∷ String
  , secret                   ∷ String
  , https                    ∷ Maybe Bool
  , repo                     ∷ FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "port"              3000                                  "The port the app should listen for connections on (for http protocol)"
    <*> simpleOption "socket"            "/var/run/sweetroll/sweetroll.sock"   "The UNIX domain socket the app should listen for connections on (for unix protocol)"
    <*> simpleOption "protocol"          "http"                                "The protocol for the server. One of: http, unix, cgi"
    <*> simpleOption "domain"            Nothing                               "The domain on which the server will run"
    <*> simpleOption "indieauthc"        Nothing                               "The IndieAuth check endpoint to use"
    <*> simpleOption "indieauthr"        Nothing                               "The IndieAuth redirect endpoint to use"
    <*> simpleOption "pushhub"           Nothing                               "The PubSubHubbub hub to use"
    <*> simpleOption "adnhost"           Nothing                               "The App.net API host to use"
    <*> simpleOption "adntoken"          ""                                    "The App.net API access token to use"
    <*> simpleOption "twihost"           Nothing                               "The Twitter API 1.1 host to use"
    <*> simpleOption "twiappkey"         ""                                    "The Twitter API application key to use"
    <*> simpleOption "twiappsecret"      ""                                    "The Twitter API application token secret to use"
    <*> simpleOption "twiacctoken"       ""                                    "The Twitter API access token to use"
    <*> simpleOption "twiaccsecret"      ""                                    "The Twitter API access token secret to use"
    <*> simpleOption "secret"            "RANDOM"                              "The JWT secret key for IndieAuth"
    <*> simpleOption "https"             Nothing                               "Whether HTTPS works on the domain"
    <*> simpleOption "repo"              "./"                                  "The git repository directory of the website"

setReset = setSGR [ Reset ]
boldYellow  x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow ] >> putStr x
boldMagenta x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta ] >> putStr x
red   x = setReset >> setSGR [ SetColor Foreground Dull Red ] >> putStr x
green x = setReset >> setSGR [ SetColor Foreground Dull Green ] >> putStr x
blue  x = setReset >> setSGR [ SetColor Foreground Dull Blue ] >> putStr x
reset x = setReset >> putStr x

putSweetroll = putStrLn "" >> putStr "  -=@@@ " >> green "Let me guess, someone stole your " >> boldYellow "sweetroll" >> green "?" >> setReset >> putStrLn " @@@=-"

optToConf ∷ AppOptions → (a → SweetrollConf → SweetrollConf) → (AppOptions → Maybe a) → SweetrollConf → SweetrollConf
optToConf o s g c = case g o of
  Just v → s v c
  _ → c

main ∷ IO ()
main = runCommand $ \opts args → do
  setCurrentDirectory $ repo opts
  let printProto = case protocol opts of
                     "http" → reset " port "   >> boldMagenta (show $ port opts)
                     "unix" → reset " socket " >> boldMagenta (show $ socket opts)
                     _      → setReset
      version = $(packageVariable $ pkgVersion . package) ++ "/" ++ $(embedGitShortRevision)
      printListening = boldYellow "     Sweetroll " >> red version >> reset " running on " >> blue (protocol opts) >> printProto >> setReset >> putStrLn ""
      warpSettings = setBeforeMainLoop printListening $ setPort (port opts) defaultSettings

  origConf ← readDocument "conf" "sweetroll" ∷ IO (Maybe SweetrollConf)
  let o = optToConf opts
      fieldMapping = [ o setDomainName                  (\x → T.pack <$> domain x)
                     , o setHttpsWorks                  https
                     , o setIndieAuthCheckEndpoint      indieauthc
                     , o setIndieAuthRedirEndpoint      indieauthr
                     , o setPushHub                     pushhub
                     , o setAdnApiHost                  adnhost
                     , o setTwitterApiHost              twihost ]
      conf = foldr ($) (fromMaybe def origConf) fieldMapping
  when (isNothing origConf) . transaction "." . saveDocument "conf" "sweetroll" $ conf

  randBytes ← getEntropy 64
  let secret' = case secret opts of
                  "RANDOM" → decodeUtf8 $ B64.encode $ H.hash randBytes
                  k → T.pack k
  let secs = def {
    secretKey                      = secret'
  , adnApiToken                    = adntoken opts
  , twitterAppKey                  = encodeUtf8 . T.pack . twiappkey    $ opts
  , twitterAppSecret               = encodeUtf8 . T.pack . twiappsecret $ opts
  , twitterAccessToken             = encodeUtf8 . T.pack . twiacctoken  $ opts
  , twitterAccessSecret            = encodeUtf8 . T.pack . twiaccsecret $ opts }

  tpls ← loadTemplates
  let app = M.sweetrollApp conf tpls secs A.app
  case protocol opts of
    "http" → putSweetroll >> app >>= runSettings warpSettings
    "unix" → putSweetroll >>
      bracket (bindPath $ socket opts)
              S.close
              (\socket → app >>= runSettingsSocket warpSettings socket)
    "cgi" → app >>= CGI.run
    _ → putStrLn $ "Unsupported protocol: " ++ protocol opts
