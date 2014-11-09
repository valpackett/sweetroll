{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE PackageImports, ImplicitParams #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util (module Sweetroll.Util) where

import           ClassyPrelude hiding (fromString, headMay)
import           Web.Scotty
import           Web.Scotty.Trans (ActionT)
import           Data.Text.Lazy (split, replace, strip)
import           Data.Char (isSpace)
import           Data.Aeson (decode)
import           Data.Stringable
import           Data.Microformats2
import "regex-pcre-builtin" Text.Regex.PCRE
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Safe (headMay)

request :: (?httpMgr :: Manager, Stringable a) => Request -> IO (Response a)
request req = do
  resp <- httpLbs req ?httpMgr
  return $ resp { responseBody = fromLazyByteString $ responseBody resp }

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
slugify :: Stringable a => a -> LText
slugify = filter (not . isSpace) . intercalate "-" . words .
          replace "&" "and" . replace "+" "plus" . replace "%" "percent" .
          replace "<" "lt" . replace ">" "gt" . replace "=" "eq" .
          replace "#" "hash" . replace "@" "at" . replace "$" "dollar" .
          filter (`notElem` ("!^*?()[]{}`./\\'\"~|" :: String)) .
          toLower . strip . toLazyText

-- | Parses comma-separated tags into a list.
--
-- >>> parseTags $ pack "article,note, first post"
-- ["article","note","first post"]
parseTags :: LText -> [LText]
parseTags = split (== ',') . replace ", " ","

-- | Removes lines that come from cpp include directive
dropIncludeCrap :: (Stringable a) => a -> a
dropIncludeCrap = fromString . unlines . filter (not . (=~ r)) . lines . toString
  where r = "#\\s+\\d+\\s+\"[^\"]+\"\\s+\\d+\\s*" :: ByteString

-- | Makes a URL from a hostname and parts
--
-- >>> mkUrl "http://localhost:4200" ["yolo", "lol"]
-- "http://localhost:4200/yolo/lol"
mkUrl :: (IsString s, Monoid s) => s -> [s] -> s
mkUrl base parts = intercalate "/" $ [base] ++ parts

type CategoryName = String
type EntrySlug = String
type SweetrollAction = ActionT LText IO

created :: LText -> SweetrollAction ()
created url = status created201 >> setHeader "Location" url

unauthorized :: SweetrollAction ()
unauthorized = status unauthorized401

-- | Encodes key-value data as application/x-www-form-urlencoded.
writeForm :: (Stringable a) => [(a, a)] -> ByteString
writeForm ps = intercalate "&" $ map (\(k, v) -> enc k ++ "=" ++ enc v) ps
  where enc = urlEncode True . toByteString

-- | Returns an action that writes data as application/x-www-form-urlencoded.
showForm :: (Stringable a) => [(a, a)] -> SweetrollAction ()
showForm x = status ok200 >> setHeader "Content-Type" "application/x-www-form-urlencoded; charset=utf-8" >> raw (toLazyByteString $ writeForm x)

derefEntry :: EntryReference -> Maybe LText
derefEntry (Left (Here c)) = citeUrl c
derefEntry (Right l) = Just l
derefEntry _ = Nothing

derefEntryName :: EntryReference -> Maybe LText
derefEntryName (Left (Here c)) = citeName c
derefEntryName (Right l) = Just l
derefEntryName _ = Nothing
