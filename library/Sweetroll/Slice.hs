{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs #-}

-- | Functions that deal with Slices (the definition of which is in Sweetroll.Pages)
module Sweetroll.Slice where

import           ClassyPrelude
import           Servant (Proxy(..), URI(..))
import           Sweetroll.Pages
import           Sweetroll.Routes
import           Sweetroll.Util

sliceCategory ∷ Int → Maybe Int → Maybe Int → CategoryName → [(Int, String)] → Slice URI
sliceCategory perPage before after catName unsortedKeys =
  let allKeys = sortOn (negate . fst) unsortedKeys
      mayFilterFst f (Just y) xs = filter (f y . fst) xs
      mayFilterFst _ Nothing  xs = xs
      items = take perPage $ mayFilterFst (<) after $ mayFilterFst (>) before allKeys in
      Slice {
        sliceItems  = map (permalink (Proxy ∷ Proxy EntryRoute) catName . snd) items
      , sliceBefore = case (fst <$> lastMay allKeys, fst <$> lastMay items) of
                        (Just minId, Just lastId) | lastId > minId → Just $ permalink (Proxy ∷ Proxy CatRouteB) catName lastId
                        _ → Nothing
      , sliceSelf   = case (before, after) of
                        (Just b, Just a)   → permalink (Proxy ∷ Proxy CatRoute)  catName b a
                        (Just b, Nothing)  → permalink (Proxy ∷ Proxy CatRouteB) catName b
                        (Nothing, Just a)  → permalink (Proxy ∷ Proxy CatRouteA) catName a
                        (Nothing, Nothing) → permalink (Proxy ∷ Proxy CatRouteE) catName
      , sliceAfter  = case (fst <$> headMay allKeys, fst <$> headMay items) of
                        (Just maxId, Just headId) | headId < maxId → Just $ permalink (Proxy ∷ Proxy CatRouteA) catName headId
                        _ → Nothing }
