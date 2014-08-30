{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

-- | Boring page rendering stuff
module Sweetroll.Pages (module Sweetroll.Pages) where

import           ClassyPrelude
import           Data.Data
import           Text.Hastache
import           Text.Hastache.Context
import           Sweetroll.Types

data Page = Page
  { name        :: LText
  , isEntryPage :: Bool }
  deriving (Eq, Show, Data, Typeable)

renderPage :: Data d => Text -> d -> IO LText
renderPage tpl dat = hastacheStr defaultConfig tpl $ mkGenericContext dat

entryPage :: Entry -> Page
entryPage e = Page { name = fromMaybe "" $ entryName e
                   , isEntryPage = True }
