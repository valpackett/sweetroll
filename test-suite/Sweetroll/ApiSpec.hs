{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Sweetroll.ApiSpec (spec) where

import           ClassyPrelude
import           Control.Lens hiding (Index, re, parts, (.=), contains)
import           Control.Monad.Trans.Writer
import           Test.Hspec
import           System.Directory
import           Data.Conduit.Shell (run, proc, conduit, ($|))
import           Data.Conduit.Combinators (sinkNull)
import           Data.Aeson.Lens
import           Data.Default
import           Data.Aeson
import qualified Network.Wai as Wai
import           Network.Wai.Test
import           Network.HTTP.Types
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
  let app = initSweetrollApp (def { testMode = Just True, domainName = Just "localhost" }) def
      transaction' = transaction "./"

  describe "GET /" $ do
    it "renders the index" $ do
      transaction' $ do
        saveNextDocument "posts" "first" $ mf2o [ "name" .= [ asText "Post 1" ] ]
        saveNextDocument "thingies" "tweeeet" $ mf2o [ "content" .= object [ "html" .= asText "Something 1" ] ]
      resp ← app >>= get "/"
      simpleBody resp `shouldSatisfy` (`contains` "posts")
      simpleBody resp `shouldSatisfy` (`contains` "thingies")
      simpleBody resp `shouldSatisfy` (`contains` "Post 1")
      simpleStatus resp `shouldBe` ok200

  describe "GET /:category" $ do
    it "renders categories" $ do
      transaction' $ do
        saveNextDocument "articles" "first"  $ mf2o [ "name" .= [ asText "First"  ] ]
        saveNextDocument "articles" "second" $ mf2o [ "name" .= [ asText "Second" ] ]
      resp ← app >>= get "/articles"
      simpleBody resp `shouldSatisfy` (`contains` "articles")
      simpleBody resp `shouldSatisfy` (`contains` "First")
      simpleBody resp `shouldSatisfy` (`contains` "Second")
      simpleStatus resp `shouldBe` ok200

  describe "GET /:category/:name" $ do
    it "returns 404 for nonexistent entries" $ do
      resp ← app >>= get "/things/not-a-thing"
      simpleStatus resp `shouldBe` notFound404

    it "returns 410 for deleted entries" $ do
      transaction' $ saveNextDocument "gonethings" "gone-thing" $ mf2o [ "name" .= [ asText "something" ] ]
      transaction' $ tell [ void $ run (proc "git" [ "rm", "gonethings/000001-gone-thing.json" ] $| conduit sinkNull) ]
      resp ← app >>= get "/gonethings/gone-thing"
      simpleStatus resp `shouldBe` gone410

    it "renders entries" $ do
      transaction' $ saveNextDocument "articles" "hello-world" $ mf2o [ "name" .= [ asText "Hello, World!" ] ]
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
      written ← readDocumentById "articles" 1 ∷ IO (Maybe Value)
      case written of
        Just article → do
          article ^. key "properties" . key "content" . nth 0 . key "html" . _String  `shouldBe` "<p>Hello</p>"
          article ^. key "properties" . key "category" . nth 0 . _String  `shouldBe` "test"
          article ^. key "properties" . key "category" . nth 1 . _String  `shouldBe` "demo"
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
