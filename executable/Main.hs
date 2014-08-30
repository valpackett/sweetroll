module Main (main) where

import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp
import           System.Console.ANSI
import           Control.Applicative
import           System.Directory
import           Sweetroll
import           Options

data AppOptions = AppOptions
  { port       :: Int
  , protocol   :: String
  , repo       :: FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "port" 3000 "The port the app should listen for connections on"
    <*> simpleOption "protocol" "http" "The protocol for the server. One of: http, cgi"
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
  let app = mkApp defaultSweetrollConf
  case protocol opts of
    "http" -> putSweetroll >> app >>= runSettings warpSettings
    "cgi" -> app >>= CGI.run
    otherwise -> putStrLn $ "Unsupported protocol: " ++ protocol opts
