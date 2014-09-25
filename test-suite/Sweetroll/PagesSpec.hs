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
  <time datetime="{{publishedAttr}}">posted {{published}}</time>
</note>{{/isNote}}
{{#isArticle}}<article>
  <h1><a href="{{permalink}}">{{name}}</a></h1>
  {{{content}}}
  <time datetime="{{publishedAttr}}">posted {{published}}</time>
</article>{{/isArticle}}
{{/isEntryPage}}|]

testCategoryTpl :: Text
testCategoryTpl = [r|{{#isEntryPage}}NOPE{{/isEntryPage}}{{#isCategoryPage}}
<category name="{{categoryName}}">
{{#entries}}<e href="{{permalink}}">{{content}}</e>
{{/entries}}</category>
{{/isCategoryPage}}|]

spec :: Spec
spec = do
  describe "renderPage" $ do
    it "renders notes" $ do
      let testNote = defaultEntry {
        entryContent      = Just $ Right "Hello, world!"
      , entryPublished    = parseISOTime "2013-10-17T09:42:49.000Z" }
      result <- renderPage testEntryTpl $ entryPage "/test" testNote
      result `shouldBe` [r|<note>
  <p>Hello, world!</p>
  <time datetime="2013-10-17 09:42">posted 17.10.2013 09:42 AM</time>
</note>|]

    it "renders articles" $ do
      let testArticle = defaultEntry {
        entryName         = Just "First post"
      , entryContent      = Just $ Right "<p>This is the content</p>"
      , entryPublished    = parseISOTime "2013-10-17T09:42:49.000Z" }
      result <- renderPage testEntryTpl $ entryPage "/articles/first" testArticle
      result `shouldBe` [r|<article>
  <h1><a href="/articles/first">First post</a></h1>
  <p>This is the content</p>
  <time datetime="2013-10-17 09:42">posted 17.10.2013 09:42 AM</time>
</article>|]

    it "renders categories" $ do
      let testCategory = [ ("f", defaultEntry { entryContent = Just $ Right "First note"  }),
                           ("s", defaultEntry { entryContent = Just $ Right "Second note" }) ]
      result <- renderPage testCategoryTpl $ categoryPage "test" testCategory
      result `shouldBe` [r|<category name="test">
<e href="/test/f">First note</e>
<e href="/test/s">Second note</e>
</category>
|]
