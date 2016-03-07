{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

-- | The definitions of pages
module Sweetroll.Pages where

import           ClassyPrelude
import           Data.Aeson.Types (Value)
import           Sweetroll.Util
import           Sweetroll.Slice
import           Sweetroll.Conf

data View α = View
  { viewConf     ∷ SweetrollConf
  , viewRenderer ∷ ByteString → Value → Text
  , viewContent  ∷ α }

data EntryPage = EntryPage CategoryName [EntrySlug] (EntrySlug, Value)

data IndexedPage = IndexedPage [CategoryName] [Slice String] (HashMap String Value)
