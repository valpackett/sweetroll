{-# LANGUAGE OverloadedStrings #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util (module Sweetroll.Util) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import           Data.Aeson (encode, decode)
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

-- | Formats a UTCTime into a text.
formatTime :: UTCTime -> Text
formatTime x = T.reverse $ T.drop 1 $ T.reverse $ T.drop 1 $ T.pack $ B8.unpack $ encode [x]

-- | Tries to find a key-value pair by key and return the value.
--
-- >>> findByKey [("cpus", 1), ("ram", 1024)] "ram"
-- Just 1024
findByKey :: Eq a => [(a, b)] -> a -> Maybe b
findByKey ps x = snd <$> find ((== x) . fst) ps

-- | Tries to find a key-value pair by key and return the value, trying all given keys.
--
-- >>> findFirstKey [("cpus", 1), ("ram", 1024)] ["hdd", "ram"]
-- Just 1024
findFirstKey :: Eq a => [(a, b)] -> [a] -> Maybe b
findFirstKey ps (x:xs) = case (findByKey ps x) of
  Just v -> Just v
  Nothing -> findFirstKey ps xs
findFirstKey _ [] = Nothing

-- | Prepares a text for inclusion in a URL.
--
-- >>> slugify $ T.pack "Hello & World!"
-- "hello-and-world"
slugify :: Text -> Text
slugify = T.intercalate "-" . T.words . T.replace "&" "and" . T.replace "+" "plus" .
          T.replace "<" "lt" . T.replace ">" "gt" . T.replace "=" "eq" .
          T.replace "#" "hash" . T.replace "@" "at" . T.replace "$" "dollar" .
          T.toLower . T.filter (`notElem` ['!', '^', '*', '?', '(', ')']) . T.strip

-- | Parses comma-separated tags into a list.
--
-- >>> parseTags $ T.pack "article,note, first post"
-- ["article","note","first post"]
parseTags :: Text -> [Text]
parseTags = T.split (== ',') . T.replace ", " ","
