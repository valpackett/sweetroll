{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util (module Sweetroll.Util) where

import           ClassyPrelude hiding (fromString, headMay)
import           Text.RawString.QQ
import           Data.Text.Lazy (replace, strip)
import           Data.Char (isSpace)
import           Data.Aeson (decode)
import           Data.Stringable hiding (length)
import           Data.Microformats2
import           Network.HTTP.Types (urlEncode)
import "regex-pcre-builtin" Text.Regex.PCRE
import           Safe (headMay)

type CategoryName = String
type EntrySlug = String

-- | Tries to parse a text ISO datetime into a UTCTime.
--
-- >>> parseISOTime "2013-10-17T09:42:49.007Z"
-- Just 2013-10-17 09:42:49.007 UTC
--
-- >>> parseISOTime "yolo"
-- Nothing
parseISOTime :: Stringable a => a -> Maybe UTCTime
parseISOTime x = headMay =<< (decodeTime $ "[\"" ++ toLazyByteString x ++ "\"]")
  where decodeTime y = decode y :: Maybe [UTCTime]

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
findFirstKey ps (x:xs) = case findByKey ps x of
  Nothing -> findFirstKey ps xs
  m -> m
findFirstKey _ [] = Nothing

-- | Prepares a text for inclusion in a URL.
--
-- >>> slugify "Hello & World!"
-- "hello-and-world"
slugify :: Stringable a => a -> a
slugify = fromLazyText . filter (not . isSpace) . intercalate "-" . words .
          replace "&" "and"  . replace "+" "plus" . replace "%" "percent" .
          replace "<" "lt"   . replace ">" "gt"   . replace "=" "eq" .
          replace "#" "hash" . replace "@" "at"   . replace "$" "dollar" .
          filter (`notElem` ("!^*?()[]{}`./\\'\"~|" :: String)) .
          toLower . strip . toLazyText

-- | Parses comma-separated tags into a list.
--
-- >>> parseTags "article,note, first post"
-- ["article","note","first post"]
-- 
-- >>> parseTags "one tag"
-- ["one tag"]
--
-- >>> parseTags ""
-- []
parseTags :: Stringable a => a -> [a]
parseTags = map fromString . matches . toString
  where matches ts = map (fromMaybe "" . headMay . drop 1) (ts =~ re :: [[String]])
        re = [r|([^,]+),?\s?|] :: ByteString

-- | Removes lines that come from cpp include directive
dropIncludeCrap :: Stringable a => a -> a
dropIncludeCrap = fromString . unlines . filter (not . (=~ re)) . lines . toString
  where re = [r|#\s+\d+\s+"[^"]+"\s+\d+\s*|] :: ByteString

-- | Makes a URL from a hostname and parts
--
-- >>> mkUrl "http://localhost:4200" ["yolo", "lol"]
-- "http://localhost:4200/yolo/lol"
mkUrl :: (IsString s, Monoid s) => s -> [s] -> s
mkUrl base parts = intercalate "/" $ [base] ++ parts

-- | Encodes key-value data as application/x-www-form-urlencoded.
writeForm :: (Stringable a) => [(a, a)] -> ByteString
writeForm ps = intercalate "&" $ map (\(k, v) -> enc k ++ "=" ++ enc v) ps
  where enc = urlEncode True . toByteString

derefEntry :: EntryReference -> Maybe LText
derefEntry (Left (Here c)) = citeUrl c
derefEntry (Right l) = Just l
derefEntry _ = Nothing

derefEntryName :: EntryReference -> Maybe LText
derefEntryName (Left (Here c)) = citeName c
derefEntryName (Right l) = Just l
derefEntryName _ = Nothing
