{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Sweetroll.RenderingSpec (spec) where

import           ClassyPrelude
import           Control.Error.Util (hush)
import           Data.Maybe (fromJust)
import           Data.Default
import           Data.Aeson
import           Text.RawString.QQ
import           Web.Simple.Templates.Language
import           Sweetroll.Pagination (paginate)
import           Sweetroll.Pages
import           Sweetroll.Rendering
import           Sweetroll.Conf
import           Test.Hspec
import           TestUtil (mf2o)

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
      let testNote = mf2o [ "content"     .= [ object [ "html" .= asText "Hello, world!" ] ]
                          , "published"   .= [ asText "2013-10-17T09:42:49.000Z" ] ]
      testRender (EntryPage "articles" [] ("first", testNote)) `shouldBe` [r|<note>
  <p>Hello, world!</p>
  <time datetime="2013-10-17T09:42Z">17.10.2013 09:42 AM</time>
</note>|]

    it "renders articles" $ do
      let testArticle = mf2o [ "name"        .= [ asText "First post" ]
                             , "content"     .= [ object [ "html" .= asText "<p>This is the content</p>" ] ]
                             , "published"   .= [ asText "2013-10-17T09:42:49.000Z" ] ]
      testRender (EntryPage "articles" [] ("first", testArticle)) `shouldBe` [r|<article>
  <h1><a href="/articles/first">First post</a></h1>
  <p>This is the content</p>
  <time datetime="2013-10-17T09:42Z">17.10.2013 09:42 AM</time>
</article>|]

    it "renders categories" $ do
      let testEntries = [ ("f", mf2o [ "content" .= [ object [ "html" .= asText "First note"  ] ] ])
                        , ("s", mf2o [ "content" .= [ object [ "html" .= asText "Second note" ] ] ]) ]
      testRender (CatPage "test" $ fromJust $ paginate False 10 1 testEntries) `shouldBe` [r|<category name="test">
<e href="/test/f">First note</e>
<e href="/test/s">Second note</e>
</category>|]

    it "renders the index" $ do
      let testCats = [ ("stuff", fromJust $ paginate False 10 1
                                 [ ("first",  mf2o [ "content" .= [ object [ "html" .= asText "First"  ] ] ])
                                 , ("second", mf2o [ "content" .= [ object [ "html" .= asText "Second" ] ] ]) ]) ]
      testRender (IndexPage testCats) `shouldBe` [r|<index>
  <category name="stuff">
    <e href="/stuff/first">First</e>
    <e href="/stuff/second">Second</e>
  </category>
</index>|]
