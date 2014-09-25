{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module SweetrollSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           System.Directory
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Network.Wai.Internal (requestMethod)
import           Network.Wai.Test
import qualified Network.Wai as Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import           Data.Microformats2
import           Text.Pandoc
import           Sweetroll.Util (findByKey)
import           Sweetroll
import           Gitson

contains :: EqSequence a => a -> a -> Bool
contains = flip isInfixOf

get  :: B.ByteString -> Wai.Application -> IO SResponse
get  = runSession . request . setPath defaultRequest { requestMethod = renderStdMethod GET }

post :: B.ByteString -> Wai.Application -> IO SResponse
post = runSession . request . setPath defaultRequest { requestMethod = renderStdMethod POST }

header :: SResponse -> String -> String
header resp x = B8.unpack $ fromMaybe "" $ findByKey (simpleHeaders resp) (CI.mk $ B8.pack x)

spec :: Spec
spec = before setup $ after cleanup $ do
  let app = mkApp defaultSweetrollConf
  let transaction' = transaction "./"

  describe "GET /:category" $ do
    it "renders categories" $ do
      transaction' $ do
        saveNextDocument "articles" "first" $ defaultEntry {
          entryName      = Just "First" }
        saveNextDocument "articles" "second" $ defaultEntry {
          entryName      = Just "Second" }
      resp <- app >>= get "/articles"
      simpleBody resp `shouldSatisfy` (`contains` "articles")
      simpleBody resp `shouldSatisfy` (`contains` "First")
      simpleBody resp `shouldSatisfy` (`contains` "Second")
      simpleStatus resp `shouldBe` ok200

  describe "GET /:category/:name" $ do
    it "returns 404 for nonexistent entries" $ do
      resp <- app >>= get "/things/not-a-thing"
      simpleStatus resp `shouldBe` notFound404

    it "renders entries" $ do
      transaction' $ saveNextDocument "articles" "hello-world" $ defaultEntry {
              entryName      = Just "Hello, World!"
            , entryAuthor    = Somewhere "/" }
      resp <- app >>= get "/articles/hello-world"
      simpleBody resp `shouldSatisfy` (`contains` "Hello, World!")
      simpleStatus resp `shouldBe` ok200

  describe "POST /micropub" $ do
    it "creates entries" $ do
      resp <- app >>= post "/micropub?h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp `shouldBe` created201
      header resp "Location" `shouldBe` "http://localhost/articles/first"
      written <- readDocumentById "articles" 1 :: IO (Maybe Entry)
      case written of
        Just article -> do
          entryContent article `shouldBe` (Just $ Left $ readMarkdown def "Hello")
          entryCategory article `shouldBe` ["test", "demo"]
        Nothing -> error "article not written"

setup :: IO ()
setup = createRepo "tmp/repo" >> setCurrentDirectory "tmp/repo"

cleanup :: IO ()
cleanup = setCurrentDirectory "../.."  >> void (try (removeDirectoryRecursive "tmp/repo") :: IO (Either IOException ()))
