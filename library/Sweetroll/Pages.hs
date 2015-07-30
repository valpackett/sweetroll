{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

-- | The definitions of pages
module Sweetroll.Pages where

import           Data.Microformats2
import           Data.Aeson.Types (Pair)
import           Sweetroll.Pagination
import           Sweetroll.Util
import           Sweetroll.Conf

data View α = View
  { viewConf     ∷ SweetrollConf
  , viewTpls     ∷ SweetrollTemplates
  , viewHostInfo ∷ [Pair]
  , viewContent  ∷ α }

data EntryPage = EntryPage CategoryName [EntrySlug] (EntrySlug, Entry)

data CatPage = CatPage CategoryName (Page (EntrySlug, Entry))

data IndexPage = IndexPage [(CategoryName, Page (EntrySlug, Entry))]
