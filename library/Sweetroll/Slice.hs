{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs #-}

-- | Functions that deal with Slices (the definition of which is in Sweetroll.Pages)
module Sweetroll.Slice where

import           ClassyPrelude
import           Servant (Proxy(..), URI(..))
import           Sweetroll.Pages
import           Sweetroll.Routes
import           Sweetroll.Util

mkCatSlice ∷ Maybe Int → Maybe Int → CategoryName → [(Int, String)] → [(Int, α)] → Slice α
mkCatSlice before after catName allKeys items =
  Slice {
    sliceItems  = map snd items
  , sliceBefore = case (fst <$> lastMay allKeys, fst <$> lastMay items) of
                    (Just minId, Just lastId) | lastId > minId →
                      Just $ permalink (Proxy ∷ Proxy CatRouteB) catName lastId
                    _ → Nothing
  , sliceSelf   = case (before, after) of
                    (Just b, Just a)   → permalink (Proxy ∷ Proxy CatRoute)  catName b a
                    (Just b, Nothing)  → permalink (Proxy ∷ Proxy CatRouteB) catName b
                    (Nothing, Just a)  → permalink (Proxy ∷ Proxy CatRouteA) catName a
                    (Nothing, Nothing) → permalink (Proxy ∷ Proxy CatRouteE) catName
  , sliceAfter  = case (fst <$> headMay allKeys, fst <$> headMay items) of
                    (Just maxId, Just headId) | headId < maxId →
                      Just $ permalink (Proxy ∷ Proxy CatRouteA) catName headId
                    _ → Nothing }

sliceCategory ∷ Int → Maybe Int → Maybe Int → CategoryName → [(Int, String)] → Slice URI
sliceCategory perPage before after catName unsortedKeys =
  mkCatSlice before after catName allKeys $ map (\(a, x) → (a, permalink (Proxy ∷ Proxy EntryRoute) catName x)) items
  where items = rangeFilter allKeys
        allKeys = sortOn (negate . fst) unsortedKeys
        rangeFilter = case (before, after) of
                        (Just b, Just a)   → take perPage . filter (\(k, _) → k < b && k > a)
                        (Just b, Nothing)  → take perPage . filter (\(k, _) → k < b)
                        (Nothing, Just a)  → reverse . take perPage . reverse . filter (\(k, _) → k > a)
                        (Nothing, Nothing) → take perPage
