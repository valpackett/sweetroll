{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Sweetroll.PagesSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           Sweetroll.Util (parseISOTime)
import           Sweetroll.Pagination (paginate)
import           Sweetroll.Pages
import           Sweetroll.Conf
import           Text.RawString.QQ
import           Data.Maybe (fromJust)
import           Data.Default()
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Web.Simple.Templates.Language

{-# ANN module ("HLint: ignore Redundant do"∷String) #-}

testEntryTpl ∷ Either String Template
testEntryTpl = compileTemplate [r|<$if(isNote)$note$else$article$endif$>$if(isNote)$
  <p>$content$</p>
$else$
  <h1><a href="$permalink$">$name$</a></h1>
  $content$
$endif$  <time datetime="$publishedAttr$">$published$</time>
</$if(isNote)$note$else$article$endif$>|]

testCategoryTpl ∷ Either String Template
testCategoryTpl = compileTemplate [r|<category name="$name$">
$for(entry in entries)$<e href="$entry.permalink$">$entry.content$</e>
$endfor$</category>|]

testIndexTpl ∷ Either String Template
testIndexTpl = compileTemplate [r|<index>
$for(cat in categories)$  <category name="$cat.name$">
$for(entry in cat.entries)$    <e href="$entry.permalink$">$entry.content$</e>
$endfor$  </category>$endfor$
</index>|]

spec ∷ Spec
spec = do
  describe "renderPage" $ do

    it "renders notes" $ do
      let testNote = def {
        entryContent      = Just $ Right "Hello, world!"
      , entryPublished    = parseISOTime ("2013-10-17T09:42:49.000Z" ∷ String) }
      testRender testEntryTpl (entryView "articles" [] ("first", testNote)) `shouldBe` [r|<note>
  <p>Hello, world!</p>
  <time datetime="2013-10-17 09:42">17.10.2013 09:42 AM</time>
</note>|]

    it "renders articles" $ do
      let testArticle = def {
        entryName         = Just "First post"
      , entryContent      = Just $ Right "<p>This is the content</p>"
      , entryPublished    = parseISOTime ("2013-10-17T09:42:49.000Z" ∷ String) }
      testRender testEntryTpl (entryView "articles" [] ("first", testArticle)) `shouldBe` [r|<article>
  <h1><a href="/articles/first">First post</a></h1>
  <p>This is the content</p>
  <time datetime="2013-10-17 09:42">17.10.2013 09:42 AM</time>
</article>|]

    it "renders categories" $ do
      let testEntries = [ ("f", def { entryContent = Just $ Right "First note"  })
                        , ("s", def { entryContent = Just $ Right "Second note" }) ]
      testRender testCategoryTpl (catView def "test" $ fromJust $ paginate False 10 1 testEntries) `shouldBe` [r|<category name="test">
<e href="/test/f">First note</e>
<e href="/test/s">Second note</e>
</category>|]

    it "renders the index" $ do
      let testCats = [ ("stuff", fromJust $ paginate False 10 1
                                 [ ("first",  def { entryContent = Just $ Right "First"  })
                                 , ("second", def { entryContent = Just $ Right "Second" }) ]) ]
      testRender testIndexTpl (indexView def testCats) `shouldBe` [r|<index>
  <category name="stuff">
    <e href="/stuff/first">First</e>
    <e href="/stuff/second">Second</e>
  </category>
</index>|]

testRender ∷ Either String Template → ViewResult → Text
testRender (Left  l) _ = error l
testRender (Right t) v = renderTemplate t mempty $ tplContext v
