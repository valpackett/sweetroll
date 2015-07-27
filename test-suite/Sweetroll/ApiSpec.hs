{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Sweetroll.ApiSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           System.Directory
import           Data.Microformats2
import           Data.Default
import           Text.Pandoc
import qualified Network.Wai as Wai
import           Network.Wai.Test
import           Network.HTTP.Types
import           Sweetroll.Util (pandocRead)
import           Sweetroll.Conf
import           Sweetroll.Api (initSweetrollApp)
import           Gitson
import           Gitson.Util (insideDirectory)
import           TestUtil

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec ∷ Spec
spec = around_ inDir $ do
  -- Storing an action that returns the app == not persisting TVar state
  -- inside the app. Fsck it, just use unsafePerformIO here :D
  -- Spent a couple hours before realizing what's been stored here :-(
  -- And then realized the TVar was not needed, because JWT.
  let app = initSweetrollApp (def { testMode = True, domainName = "localhost" }) def def
      transaction' = transaction "./"

  describe "GET /" $ do
    it "renders the index" $ do
      transaction' $ do
        saveNextDocument "posts" "first" $ def {
          entryName      = pure "Post 1" }
        saveNextDocument "thingies" "tweeeet" $ def {
          entryContent   = pure . PandocContent . (pandocRead readMarkdown) $ "Something 1" }
      resp ← app >>= get "/"
      simpleBody resp `shouldSatisfy` (`contains` "posts")
      simpleBody resp `shouldSatisfy` (`contains` "thingies")
      simpleBody resp `shouldSatisfy` (`contains` "Post 1")
      simpleBody resp `shouldSatisfy` (`contains` "Something 1")
      simpleStatus resp `shouldBe` ok200

  describe "GET /:category" $ do
    it "renders categories" $ do
      transaction' $ do
        saveNextDocument "articles" "first" $ def {
          entryName      = pure "First" }
        saveNextDocument "articles" "second" $ def {
          entryName      = pure "Second" }
      resp ← app >>= get "/articles"
      simpleBody resp `shouldSatisfy` (`contains` "articles")
      simpleBody resp `shouldSatisfy` (`contains` "First")
      simpleBody resp `shouldSatisfy` (`contains` "Second")
      simpleStatus resp `shouldBe` ok200

  describe "GET /:category/:name" $ do
    it "returns 404 for nonexistent entries" $ do
      resp ← app >>= get "/things/not-a-thing"
      simpleStatus resp `shouldBe` notFound404

    it "renders entries" $ do
      transaction' $ saveNextDocument "articles" "hello-world" $ def {
              entryName      = pure "Hello, World!"
            , entryAuthor    = pure . TextCard $ "/" }
      resp ← app >>= get "/articles/hello-world"
      simpleBody resp `shouldSatisfy` (`contains` "Hello, World!")
      simpleStatus resp `shouldBe` ok200

--     it "returns last-modified / 304" $ do
--       transaction' $ saveNextDocument "articles" "hello-cache" $ def {
--               entryUpdated = pure $ UTCTime (fromGregorian 2015 1 2) 0 }
--       resp1 ← app >>= get "/articles/hello-cache"
--       header resp1 "Last-Modified" `shouldBe` "Fri, 02 Jan 2015 00:00:00 GMT"
--       resp2 ← app >>= get' defaultRequest { Wai.requestHeaders = [("If-Modified-Since", "Thu, 01 Jan 2015 00:00:00 GMT")] } "/articles/hello-cache"
--       simpleStatus resp2 `shouldBe` ok200
--       header resp2 "Last-Modified" `shouldBe` "Fri, 02 Jan 2015 00:00:00 GMT"
--       resp3 ← app >>= get' defaultRequest { Wai.requestHeaders = [("If-Modified-Since", "Sat, 03 Jan 2015 00:00:00 GMT")] } "/articles/hello-cache"
--       simpleStatus resp3 `shouldBe` notModified304
--       header resp3 "Last-Modified" `shouldBe` "Fri, 02 Jan 2015 00:00:00 GMT"

  describe "POST /micropub" $ do
    it "creates entries" $ do
      resp ← app >>= postAuthed (defaultRequest { Wai.requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded") ] })
                                "/micropub" "h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp `shouldBe` created201
      header resp "Location" `shouldBe` "http://localhost/articles/first"
      written ← readDocumentById "articles" 1 ∷ IO (Maybe Entry)
      case written of
        Just article → do
          entryContent article `shouldBe` (pure . PandocContent . (pandocRead readMarkdown) $ "Hello")
          entryCategory article `shouldBe` ["test", "demo"]
        Nothing → error "article not written"

    it "requires auth" $ do
      resp ← app >>= post' (defaultRequest { Wai.requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded") ] })
                           "/micropub" "h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp `shouldBe` unauthorized401
      resp' ← app >>= post' (defaultRequest { Wai.requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded")
                                                                  , ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJtZSIsImlzcyI6ImxvY2FsaG9zdCIsImlhdCI6MTQzODAxNjU5N30.KQRooUPpnhlhA0_xRbHF8q4AesUu5x6QNoVUuFavVng") ] })
                           "/micropub" "h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp' `shouldBe` unauthorized401

inDir ∷ IO () → IO ()
inDir x = createRepo dirPath >> insideDirectory dirPath x >> cleanup
  where cleanup = void (try (removeDirectoryRecursive dirPath) ∷ IO (Either IOException ()))
        dirPath = "tmp/repo"
