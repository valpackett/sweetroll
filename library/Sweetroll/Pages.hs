{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

-- | The definitions of pages
module Sweetroll.Pages where

import           Data.Aeson.Types (Pair, Value)
import           Sweetroll.Pagination
import           Sweetroll.Util
import           Sweetroll.Conf

data View α = View
  { viewConf     ∷ SweetrollConf
  , viewTpls     ∷ SweetrollTemplates
  , viewHostInfo ∷ [Pair]
  , viewContent  ∷ α }

data EntryPage = EntryPage CategoryName [EntrySlug] (EntrySlug, Value)

data CatPage = CatPage CategoryName (Page (EntrySlug, Value))

data IndexPage = IndexPage [(CategoryName, Page (EntrySlug, Value))]
