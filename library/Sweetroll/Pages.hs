{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

-- | The module responsible for rendering pages into actual HTML
module Sweetroll.Pages (module Sweetroll.Pages) where

import           ClassyPrelude
import           Text.Hastache.Context
import           Text.Hastache
import           Text.Pandoc
import           Text.Blaze.Renderer.Text
import           Data.Microformats2
import           Data.Data

renderPage :: Data d => Text -> d -> IO LText
renderPage tpl dat = hastacheStr defaultConfig tpl $ mkGenericContext dat

data EntryPage = EntryPage
  { name                :: LText
  , content             :: LText
  , published           :: LText
  , publishedAttr       :: LText
  , permalink           :: LText
  , isEntryPage         :: Bool
  , isNote              :: Bool
  , isArticle           :: Bool }
  deriving (Eq, Show, Data, Typeable)

entryPage :: LText -> Entry -> EntryPage
entryPage l e = EntryPage
  { name               = fromMaybe "" $ entryName e
  , content            = renderContent e
  , published          = fromMaybe "" $ formatTimeText <$> entryPublished e
  , publishedAttr      = fromMaybe "" $ formatTimeAttr <$> entryPublished e
  , permalink          = l
  , isEntryPage        = True
  , isNote             = isNothing $ entryName e
  , isArticle          = isJust $ entryName e }
    where formatTimeText = pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"
          formatTimeAttr = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

data CategoryPage = CategoryPage
  { entries             :: [EntryPage]
  , categoryName        :: LText
  , isCategoryPage      :: Bool }
  deriving (Eq, Show, Data, Typeable)

categoryPage :: LText -> [(LText, Entry)] -> CategoryPage
categoryPage n es = CategoryPage
  { entries             = map mkEntryPage es
  , categoryName        = n
  , isCategoryPage      = True }
    where mkEntryPage (slug, e) = entryPage (mconcat ["/", n, "/", slug]) e

renderContent :: Entry -> LText
renderContent e = case fromMaybe (Right "") $ entryContent e of
  Left p -> renderMarkup $ writeHtml def p
  Right t -> t
