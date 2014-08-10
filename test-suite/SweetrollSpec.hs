{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module SweetrollSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           System.Directory
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Network.Wai.Internal (Request, requestMethod)
import           Network.Wai.Test
import qualified Network.Wai as Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import           Sweetroll.Types
import           Sweetroll.Util (findByKey)
import           Sweetroll
import           Gitson

post :: B.ByteString -> Wai.Application -> IO SResponse
post = runSession . request . setPath defaultRequest { requestMethod = renderStdMethod POST }

header :: SResponse -> String -> String
header resp x = B8.unpack $ fromMaybe "" $ findByKey (simpleHeaders resp) (CI.mk $ B8.pack x)

spec :: Spec
spec = before setup $ after cleanup $ do
  describe "POST /micropub" $ do
    it "creates notes" $ do
      resp <- app >>= post "/micropub?h=entry&slug=first&content=Hello&tags=test,demo"
      simpleStatus resp `shouldBe` created201
      header resp "Location" `shouldBe` "http://localhost/notes/first"
      written <- readEntryById "notes" 1 :: IO (Maybe Entry)
      case written of
        Just note -> do
          entryContent note `shouldBe` Just "Hello"
          entryTags note `shouldBe` ["test", "demo"]
        Nothing -> error "Note not written"

setup :: IO ()
setup = createRepo "tmp/repo" >> setCurrentDirectory "tmp/repo"

cleanup :: IO ()
cleanup = setCurrentDirectory "../.."  >> void (try (removeDirectoryRecursive "tmp/repo") :: IO (Either IOException ()))
