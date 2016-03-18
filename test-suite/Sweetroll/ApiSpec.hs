{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Sweetroll.ApiSpec (spec) where

import           Sweetroll.Prelude hiding (Index, re, parts, contains)
import           Control.Monad.Trans.Writer
import           Control.Concurrent
import           Test.Hspec
import           System.Directory
import           Data.Conduit.Shell (run, proc, conduit, ($|))
import           Data.Conduit.Combinators (sinkNull)
import           Data.Maybe (fromJust)
import qualified Network.Wai as Wai
import           Network.Wai.Test
import qualified Network.Wai.Handler.Warp as Warp
import           Sweetroll.Conf
import           Sweetroll.Api (initSweetrollApp)
import           Gitson
import           Gitson.Util (insideDirectory)
import           TestUtil

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

app ∷ IO Wai.Application
app = initSweetrollApp (def { testMode = Just True, domainName = Just "localhost:8998" }) def

spec ∷ Spec
spec = around_ inDir $ around_ withServer $ do
  -- Storing an action that returns the app == not persisting TVar state
  -- inside the app. Fsck it, just use unsafePerformIO here :D
  -- Spent a couple hours before realizing what's been stored here :-(
  -- And then realized the TVar was not needed, because JWT.
  let transaction' = transaction "./"

  describe "GET /" $ do
    it "renders the index" $ do
      transaction' $ do
        saveNextDocument "articles" "first" $ mf2o [ "name" .= [ asText "Post 1" ] ]
        saveNextDocument "notes" "tweeeet" $ mf2o [ "content" .= object [ "html" .= asText "Something 1" ] ]
      resp ← app >>= get "/"
      simpleBody resp `shouldSatisfy` (`contains` "Post 1")
      simpleBody resp `shouldSatisfy` (`contains` "Something 1")
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
      transaction' $ saveNextDocument "notes" "reply-target" $ mf2o [ "content" .= [ asText "Hello, World!" ] ]
      resp ← app >>= postAuthed formRequest "/micropub" "h=entry&name=First&slug=first&content=Hello&category[]=test&category[]=demo&in-reply-to=http://localhost:8998/notes/reply-target"
      simpleStatus resp `shouldBe` created201
      header resp "Location" `shouldBe` "http://localhost:8998/articles/first"
      written ← readDocumentByName "articles" "first" ∷ IO (Maybe Value)
      case written of
        Just article → do
          article ^. key "properties" . key "content" . nth 0 . key "html" . _String  `shouldBe` "<p>Hello</p>"
          article ^. key "properties" . key "category" . nth 0 . _String  `shouldBe` "demo"
          article ^. key "properties" . key "category" . nth 1 . _String  `shouldBe` "test"
        Nothing → error "article not written"

    it "requires auth" $ do
      resp ← app >>= post' formRequest "/micropub" "h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp `shouldBe` unauthorized401
      resp' ← app >>= post' (defaultRequest { Wai.requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded")
                                                                   , ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJtZSIsImlzcyI6ImxvY2FsaG9zdCIsImlhdCI6MTQzODAxNjU5N30.KQRooUPpnhlhA0_xRbHF8q4AesUu5x6QNoVUuFavVng") ] })
                           "/micropub" "h=entry&name=First&slug=first&content=Hello&category=test,demo"
      simpleStatus resp' `shouldBe` unauthorized401

  let makeMentionPosts = transaction' $ do
        saveNextDocument "notes" "reply-target" $ mf2o [ "content" .= [ asText "Hello, World!" ] ]
        saveNextDocument "notes" "other-reply-target" $ mf2o [ "content" .= [ asText "World, Hello!" ] ]
        saveNextDocument "replies" "reply-to-target" $ mf2o [ "content" .= [ asText "Yo" ], "in-reply-to" .= [ asText "http://localhost:8998/notes/reply-target" ] ]
        saveNextDocument "replies" "reply-to-nonexistent" $ mf2o [ "content" .= [ asText "NOPE" ], "in-reply-to" .= [ asText "http://localhost:8998/notes/not-reply-target" ] ]
        saveNextDocument "replies" "reply-to-other" $ mf2o [ "content" .= [ asText "N0pe" ], "in-reply-to" .= [ asText "http://localhost:8998/notes/other-reply-target" ] ]

  describe "POST /webmention" $ before_ makeMentionPosts $ do
    let expectTargetToHaveOneMentionWithText t = do
          target ← readDocumentByName "notes" "reply-target" ∷ IO (Maybe Value)
          let comments x = (fromJust target) ^? key "properties" . key "comment" . x
          length <$> comments _Array `shouldBe` Just 1
          comments (nth 0 . key "properties" . key "url" . nth 0 . _String) `shouldBe` Just "http://localhost:8998/replies/reply-to-target"
          comments (nth 0 . key "properties" . key "content" . nth 0 . key "html" . _String) `shouldBe` Just t
        expectTargetToHaveNoMentions = do
          target ← readDocumentByName "notes" "reply-target" ∷ IO (Maybe Value)
          let comments x = (fromJust target) ^? key "properties" . key "comment" . x
          (length <$> comments _Array) `shouldSatisfy` (`elem` [Just 0, Nothing])
        expectTargetToHaveOneMentionAndTombstone t = do
          target ← readDocumentByName "notes" "reply-target" ∷ IO (Maybe Value)
          let comments x = (fromJust target) ^? key "properties" . key "comment" . x
          length <$> comments _Array `shouldBe` Just 2
          comments (nth 0 . key "properties" . key "url" . nth 0 . _String) `shouldBe` Just "http://localhost:8998/replies/reply-to-target"
          comments (nth 0 . key "properties" . key "content" . nth 0 . key "html" . _String) `shouldBe` Just t
          comments (nth 1 . key "properties" . key "content" . nth 0 . key "html" . _String) `shouldBe` Just "This entry has been deleted."



    it "saves, updates and deletes correct replies" $ do
      resp ← app >>= postAuthed formRequest "/webmention" "source=http://localhost:8998/replies/reply-to-target&target=http://localhost:8998/notes/reply-target"
      simpleStatus resp `shouldBe` accepted202
      expectTargetToHaveOneMentionWithText "\n\t\t\t\t\n\t\t\t\t\n\nYo\n\n\t\t\t\t\n\t\t\t"
      transaction' $ saveDocumentByName "replies" "reply-to-target" $ mf2o [ "content" .= [ asText "HELLO" ], "in-reply-to" .= [ asText "http://localhost:8998/notes/reply-target" ] ]
      void $ app >>= postAuthed formRequest "/webmention" "source=http://localhost:8998/replies/reply-to-target&target=http://localhost:8998/notes/reply-target"
      expectTargetToHaveOneMentionWithText " HELLO "
      transaction' $ tell [ void $ run (proc "git" [ "rm", "replies/000001-reply-to-target.json" ] $| conduit sinkNull) ]
      void $ app >>= postAuthed formRequest "/webmention" "source=http://localhost:8998/replies/reply-to-target&target=http://localhost:8998/notes/reply-target"
      expectTargetToHaveOneMentionAndTombstone " HELLO "

    it "rejects replies to other things" $ do
      resp ← app >>= postAuthed formRequest "/webmention" "source=http://localhost:8998/replies/reply-to-nonexistent&target=http://localhost:8998/notes/reply-target"
      simpleStatus resp `shouldBe` accepted202
      resp1 ← app >>= postAuthed formRequest "/webmention" "source=http://localhost:8998/replies/reply-to-other&target=http://localhost:8998/notes/reply-target"
      simpleStatus resp1 `shouldBe` accepted202
      expectTargetToHaveNoMentions


-- needed for self-webmentioning basically
withServer ∷ IO () → IO ()
withServer x = do
  tid ← forkOS $ Warp.run 8998 =<< app
  -- have to kill the thread when the exception happens to avoid "address in use"
  -- if you don't rethrow here, all tests will always pass!!! :D
  void $ catchAny x $ \e → killThread tid >> throwM e
  killThread tid

inDir ∷ IO () → IO ()
inDir x = createRepo dirPath >> insideDirectory dirPath x >> cleanup
  where cleanup = void (try (removeDirectoryRecursive dirPath) ∷ IO (Either IOException ()))
        dirPath = "tmp/repo"
