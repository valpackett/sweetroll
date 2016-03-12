{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs #-}

-- | Slices are results of pagination, basically.
module Sweetroll.Slice where

import           Sweetroll.Prelude

data Slice α = Slice
  { sliceItems   ∷ [(Int, α)]
  , sliceBefore  ∷ Maybe Int
  , sliceCatName ∷ CategoryName
  , sliceAfter   ∷ Maybe Int }
  deriving (Show, Eq)

mkCatSlice ∷ CategoryName → [Int] → [(Int, α)] → Slice α
mkCatSlice catName allIds items =
  Slice {
    sliceItems   = items
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
                        (Just b, Just a)   → filter (\(k, _) → k < b && k > a)
                        (Just b, Nothing)  → take perPage . filter (\(k, _) → k < b)
                        (Nothing, Just a)  → reverse . take perPage . reverse . filter (\(k, _) → k > a)
                        (Nothing, Nothing) → take perPage

mergeSlices ∷ Eq α ⇒ Int → Bool → Slice α → Slice α → Slice α
mergeSlices perPage isAfter x y =
  Slice {
    sliceItems    = items
  , sliceBefore   = if isJust (sliceBefore x) || isJust (sliceBefore y) || (not isAfter && length allItems > perPage)
                       then (fst <$> lastMay items)
                       else Nothing
  , sliceCatName  = sliceCatName y ++ "+" ++ sliceCatName x
  , sliceAfter    = if isJust (sliceAfter x) || isJust (sliceAfter y) || (isAfter && length allItems > perPage)
                       then (fst <$> headMay items)
                       else Nothing }
    where items = (if isAfter then reverse . take perPage . reverse else take perPage)
                  (sortOn (negate . fst) allItems)
          allItems = sliceItems x ++ sliceItems y
