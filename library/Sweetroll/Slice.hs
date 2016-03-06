{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs #-}

-- | Slices are results of pagination, basically.
module Sweetroll.Slice where

import           ClassyPrelude
import           Sweetroll.Util

data Slice α = Slice
  { sliceItems   ∷ [α]
  , sliceBefore  ∷ Maybe Int
  , sliceCatName ∷ CategoryName
  , sliceAfter   ∷ Maybe Int }

-- TODO: instance Semigroup Slice where

mkCatSlice ∷ CategoryName → [Int] → [(Int, α)] → Slice α
mkCatSlice catName allIds items =
  Slice {
    sliceItems   = map snd items
  , sliceBefore  = case (lastMay allIds, fst <$> lastMay items) of
                     (Just minId, Just lastId) | lastId > minId →
                       Just lastId
                     _ → Nothing
  , sliceCatName = catName
  , sliceAfter   = case (headMay allIds, fst <$> headMay items) of
                     (Just maxId, Just headId) | headId < maxId →
                       Just headId
                     _ → Nothing }

sliceCategory ∷ Int → Maybe Int → Maybe Int → CategoryName → [(Int, String)] → Slice String
sliceCategory perPage before after catName unsortedKeys =
  mkCatSlice catName (map fst allKeys) $ map (\(a, x) → (a, "/" ++ catName ++ "/" ++ x)) items
  where items = rangeFilter allKeys
        allKeys = sortOn (negate . fst) unsortedKeys
        rangeFilter = case (before, after) of
                        (Just b, Just a)   → take perPage . filter (\(k, _) → k < b && k > a)
                        (Just b, Nothing)  → take perPage . filter (\(k, _) → k < b)
                        (Nothing, Just a)  → reverse . take perPage . reverse . filter (\(k, _) → k > a)
                        (Nothing, Nothing) → take perPage
