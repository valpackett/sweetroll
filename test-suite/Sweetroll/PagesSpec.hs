{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Sweetroll.PagesSpec (spec) where

import           ClassyPrelude
import           Control.Error.Util (hush)
import           Data.Maybe (fromJust)
import           Data.Default()
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Text.RawString.QQ
import           Web.Simple.Templates.Language
import           Sweetroll.Util (parseISOTime)
import           Sweetroll.Pagination (paginate)
import           Sweetroll.Pages
import           Sweetroll.Conf
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

testEntryTpl, testCategoryTpl, testIndexTpl ∷ Either String Template
testEntryTpl = compileTemplate [r|<$if(isUntitled)$note$else$article$endif$>$if(isUntitled)$
  <p>$content$</p>
$else$
  <h1><a href="$permalink$">$name$</a></h1>
  $content$
$endif$  <time datetime="$publishedAttr$">$published$</time>
</$if(isUntitled)$note$else$article$endif$>|]

testCategoryTpl = compileTemplate [r|<category name="$name$">
$for(entry in entries)$<e href="$entry.permalink$">$entry.content$</e>
$endfor$</category>|]

testIndexTpl = compileTemplate [r|<index>
$for(cat in categories)$  <category name="$cat.name$">
$for(entry in cat.entries)$    <e href="$entry.permalink$">$entry.content$</e>
$endfor$  </category>$endfor$
</index>|]

testRender ∷ Templatable α ⇒ α → Text
testRender x = resultHtml $ renderBare (templateInLayout v) v
  where v = View def tpls [] x
        tpls = def { entryTemplate    = fromJust $ hush testEntryTpl
                   , categoryTemplate = fromJust $ hush testCategoryTpl
                   , indexTemplate    = fromJust $ hush testIndexTpl }

spec ∷ Spec
spec = do
  describe "renderPage" $ do

    it "renders notes" $ do
      let testNote = def {
        entryContent      = pure . TextContent $ "Hello, world!"
      , entryPublished    = maybeToList . parseISOTime $ ("2013-10-17T09:42:49.000Z" ∷ String) }
      testRender (EntryPage "articles" [] ("first", testNote)) `shouldBe` [r|<note>
  <p>Hello, world!</p>
  <time datetime="2013-10-17 09:42">17.10.2013 09:42 AM</time>
</note>|]

    it "renders articles" $ do
      let testArticle = def {
        entryName         = pure "First post"
      , entryContent      = pure . TextContent $ "<p>This is the content</p>"
      , entryPublished    = maybeToList . parseISOTime $ ("2013-10-17T09:42:49.000Z" ∷ String) }
      testRender (EntryPage "articles" [] ("first", testArticle)) `shouldBe` [r|<article>
  <h1><a href="/articles/first">First post</a></h1>
  <p>This is the content</p>
  <time datetime="2013-10-17 09:42">17.10.2013 09:42 AM</time>
</article>|]

    it "renders categories" $ do
      let testEntries = [ ("f", def { entryContent = pure . TextContent $ "First note"  })
                        , ("s", def { entryContent = pure . TextContent $ "Second note" }) ]
      testRender (CatPage "test" $ fromJust $ paginate False 10 1 testEntries) `shouldBe` [r|<category name="test">
<e href="/test/f">First note</e>
<e href="/test/s">Second note</e>
</category>|]

    it "renders the index" $ do
      let testCats = [ ("stuff", fromJust $ paginate False 10 1
                                 [ ("first",  def { entryContent = pure . TextContent $ "First"  })
                                 , ("second", def { entryContent = pure . TextContent $ "Second" }) ]) ]
      testRender (IndexPage testCats) `shouldBe` [r|<index>
  <category name="stuff">
    <e href="/stuff/first">First</e>
    <e href="/stuff/second">Second</e>
  </category>
</index>|]
