{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

-- | Boring page rendering stuff
module Sweetroll.Pages (module Sweetroll.Pages) where

import           ClassyPrelude
import           Text.Hastache.Context
import           Text.Hastache
import           Data.Microformats2
import           Data.Data

data Page = Page
  { name        :: LText
  , isEntryPage :: Bool }
  deriving (Eq, Show, Data, Typeable)

renderPage :: Data d => Text -> d -> IO LText
renderPage tpl dat = hastacheStr defaultConfig tpl $ mkGenericContext dat

entryPage :: Entry -> Page
entryPage e = Page { name = fromMaybe "" $ entryName e
                   , isEntryPage = True }
