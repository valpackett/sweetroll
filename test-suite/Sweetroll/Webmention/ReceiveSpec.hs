{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Sweetroll.Webmention.ReceiveSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           Data.Aeson.QQ
import           Sweetroll.Webmention.Receive

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec ∷ Spec
spec = do
  describe "upsertMention" $ do

    it "replaces first-level replies" $ do
      upsertMention [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["url"], "data": ["old-data"] }},
                      {"properties":{ "url": ["not-url"], "data": ["some-data"] }}
                    ]}}|]
                    [aesonQQ|{"properties":{ "url": ["url"], "data": ["new-data"] }}|]
                  `shouldBe`
                    [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["url"], "data": ["new-data"] }},
                      {"properties":{ "url": ["not-url"], "data": ["some-data"] }}
                    ]}}|]

    it "replaces nested replies" $ do
      upsertMention [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["not-url"], "data": ["some-data"],
                       "comment": [
                        {"properties":{ "url": ["url"], "data": ["old-data"] }}
                       ]}}
                    ]}}|]
                    [aesonQQ|{"properties":{ "url": ["url"], "data": ["new-data"] }}|]
                  `shouldBe`
                    [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["not-url"], "data": ["some-data"],
                       "comment": [
                        {"properties":{ "url": ["url"], "data": ["new-data"] }}
                       ]}}
                    ]}}|]

    it "adds new replies" $ do
      upsertMention [aesonQQ|{"properties":{}}|]
                    [aesonQQ|{"properties":{ "url": ["url"], "data": ["new-data"] }}|]
                  `shouldBe`
                    [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["url"], "data": ["new-data"] }}
                    ]}}|]
      upsertMention [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["other-url"], "data": ["old-data"] }}
                    ]}}|]
                    [aesonQQ|{"properties":{ "url": ["url"], "data": ["new-data"] }}|]
                  `shouldBe`
                    [aesonQQ|{"properties":{"comment":[
                      {"properties":{ "url": ["other-url"], "data": ["old-data"] }},
                      {"properties":{ "url": ["url"], "data": ["new-data"] }}
                    ]}}|]
