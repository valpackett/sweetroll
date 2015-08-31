{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

-- | The definitions of pages
module Sweetroll.Pages where

import           ClassyPrelude
import           Data.Aeson.Types (Value)
import           Network.URI (URI)
import           Sweetroll.Util
import           Sweetroll.Conf

data View α = View
  { viewConf     ∷ SweetrollConf
  , viewRenderer ∷ ByteString → Value → Text
  , viewContent  ∷ α }

data Slice α = Slice
  { sliceItems  ∷ [α]
  , sliceBefore ∷ Maybe URI
  , sliceSelf   ∷ URI
  , sliceAfter  ∷ Maybe URI }

data EntryPage = EntryPage CategoryName [EntrySlug] (EntrySlug, Value)

data CatPage = CatPage CategoryName (Slice (EntrySlug, Value))

data IndexPage = IndexPage [(CategoryName, Slice (EntrySlug, Value))]
