{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp
import           System.Console.ANSI
import           Control.Applicative
import           System.Directory
import           Sweetroll.Conf
import           Sweetroll.App
import qualified Data.Text as T
import           Options

data AppOptions = AppOptions
  { port          :: Int
  , protocol      :: String
  , domain        :: String
  , sitename      :: String
  , indieauth     :: String
  , https         :: Bool
  , repo          :: FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "port" 3000 "The port the app should listen for connections on"
    <*> simpleOption "protocol" "http" "The protocol for the server. One of: http, cgi"
    <*> simpleOption "domain" "localhost:3000" "The domain on which the server will run"
    <*> simpleOption "sitename" "A new Sweetroll website" "The name of the website"
    <*> simpleOption "indieauth" "https://indieauth.com/auth" "The IndieAuth endpoint to use"
    <*> simpleOption "https" False "Whether HTTPS works on the domain"
    <*> simpleOption "repo" "./" "The git repository directory of the website"

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
  let printListening = boldYellow "         Sweetroll " >> red "0.0.0" >> reset " running on " >> blue (protocol opts) >> reset " port " >> boldMagenta (show $ port opts) >> setReset >> putStrLn ""
  let warpSettings = setBeforeMainLoop printListening $ setPort (port opts) defaultSettings
  conf <- loadTemplates defaultSweetrollConf {
    domainName = case domain opts of
      "" -> Nothing
      s -> Just $ T.pack s
  , httpsWorks = https opts
  , siteName = T.pack $ sitename opts
  , indieAuthEndpoint = indieauth opts
  }
  let app = mkApp conf
  case protocol opts of
    "http" -> putSweetroll >> app >>= runSettings warpSettings
    "cgi" -> app >>= CGI.run
    _ -> putStrLn $ "Unsupported protocol: " ++ protocol opts
