{-# LANGUAGE OverloadedStrings #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util (module Sweetroll.Util) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import           Data.Aeson (decode)
import           Data.List (find)
import           Control.Applicative

-- | Tries to parse a text ISO datetime into a UTCTime.
--
-- >>> import Data.Text.Lazy
-- >>> parseTime $ Just $ pack "2013-10-17T09:42:49.007Z"
-- Just 2013-10-17 09:42:49.007 UTC
--
-- >>> parseTime $ Just $ pack "yolo"
-- Nothing
parseTime :: Maybe Text -> Maybe UTCTime
parseTime x = fmap head $ decodeTime $ B8.pack $ T.unpack $ mconcat ["[\"", (fromMaybe "" x), "\"]"]
  where decodeTime y = (decode y) :: Maybe [UTCTime]


-- | Tries to find a key-value pair by key and return the value.
findByKey :: Eq a => [(a, b)] -> a -> Maybe b
findByKey ps x = snd <$> find ((== x) . fst) ps
