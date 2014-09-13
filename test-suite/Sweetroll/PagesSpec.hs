{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Sweetroll.PagesSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           Sweetroll.Pages
import           Sweetroll.Util (parseISOTime)
import           Text.RawString.QQ
import           Data.Microformats2

testEntryTpl :: Text
testEntryTpl = [r|{{#isEntryPage}}
{{#isNote}}<note>
  <p>{{content}}</p>
  <time datetime="{{publishedISO}}">posted {{published}}</time>
</note>{{/isNote}}
{{#isArticle}}<article>
  <h1>{{name}}</h1>
  {{{content}}}
  <time datetime="{{publishedISO}}">posted {{published}}</time>
</article>{{/isArticle}}
{{/isEntryPage}}|]

spec :: Spec
spec = do
  describe "renderPage with entryPage" $ do
    it "renders notes" $ do
      let testNote = defaultEntry {
        entryContent      = Just $ Right "Hello, world!"
      , entryPublished    = parseISOTime "2013-10-17T09:42:49.000Z" }
      result <- renderPage testEntryTpl $ entryPage testNote
      result `shouldBe` [r|<note>
  <p>Hello, world!</p>
  <time datetime="2013-10-17T09:42:49.000Z">posted 17.10.2013 09:42 AM</time>
</note>|]

    it "renders articles" $ do
      let testArticle = defaultEntry {
        entryName         = Just "First post"
      , entryContent      = Just $ Right "<p>This is the content</p>"
      , entryPublished    = parseISOTime "2013-10-17T09:42:49.000Z" }
      result <- renderPage testEntryTpl $ entryPage testArticle
      result `shouldBe` [r|<article>
  <h1>First post</h1>
  <p>This is the content</p>
  <time datetime="2013-10-17T09:42:49.000Z">posted 17.10.2013 09:42 AM</time>
</article>|]
