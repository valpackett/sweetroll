{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

-- | Boring page rendering stuff
module Sweetroll.Pages (module Sweetroll.Pages) where

import           ClassyPrelude
import           Sweetroll.Util (formatISOTime)
import           Text.Hastache.Context
import           Text.Hastache
import           Text.Pandoc
import           Text.Blaze.Renderer.Text
import           Data.Microformats2
import           Data.Data

data Page = Page
  { name                :: LText
  , content             :: LText
  , published           :: LText
  , publishedISO        :: LText
  , isEntryPage         :: Bool
  , isNote              :: Bool
  , isArticle           :: Bool }
  deriving (Eq, Show, Data, Typeable)

renderPage :: Data d => Text -> d -> IO LText
renderPage tpl dat = hastacheStr defaultConfig tpl $ mkGenericContext dat

formatDateTime :: UTCTime -> LText
formatDateTime = pack . formatTime defaultTimeLocale "%d.%m.%Y %I:%M %p"

entryPage :: Entry -> Page
entryPage e = Page
  { name               = fromMaybe "" $ entryName e
  , content            = case fromMaybe (Right "") $ entryContent e of
                           Left p -> renderMarkup $ writeHtml def p
                           Right t -> t
  , published          = fromMaybe "" $ formatDateTime <$> entryPublished e
  , publishedISO       = fromMaybe "" $ formatISOTime <$> entryPublished e
  , isEntryPage        = True
  , isNote             = entryName e == Nothing
  , isArticle          = entryName e /= Nothing}
