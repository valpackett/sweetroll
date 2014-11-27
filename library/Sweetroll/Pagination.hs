{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | The pagination algorithm used by Sweetroll.
module Sweetroll.Pagination (module Sweetroll.Pagination) where

import           ClassyPrelude

data Page a = Page {
    items     :: [a]
  , firstPage :: Int
  , prevPage  :: Maybe Int
  , thisPage  :: Int
  , nextPage  :: Maybe Int
  , lastPage  :: Int
  } deriving (Show, Eq)

changeItems :: Page a -> [b] -> Page b
changeItems (Page _ f p t n l) nI = Page nI f p t n l

-- | Paginates a list of items.
--
-- >>> paginate False 10 1 [1..5]
-- Just (Page {items = [1,2,3,4,5], firstPage = 1, prevPage = Nothing, thisPage = 1, nextPage = Nothing, lastPage = 1})
--
-- >>> paginate True 10 4 [1..50]
-- Just (Page {items = [40,39,38,37,36,35,34,33,32,31], firstPage = 1, prevPage = Just 3, thisPage = 4, nextPage = Just 5, lastPage = 5})
--
-- >>> paginate False 10 6 [1..50]
-- Nothing
paginate :: Bool -> Int -> Int -> [a] -> Maybe (Page a)
paginate isReverse perPage pageNumber allItems =
  if length its > 0 then
    Just $ Page its 1 prv' pgn nxt' lst
  else Nothing
  where lst  = (1 +) $ (length allItems - 1) `div` perPage
        pgn  = if pageNumber == -1 then lst else pageNumber
        prv  = pgn - 1
        prv' = if prv > 0    then Just prv else Nothing
        nxt  = pgn + 1
        nxt' = if nxt <= lst then Just nxt else Nothing
        its  = take perPage $ if isReverse then
                 drop (perPage * (lst - 1 - prv)) $ reverse allItems
               else
                 drop (perPage * prv) allItems
