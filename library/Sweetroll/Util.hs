{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GADTs #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util (module Sweetroll.Util) where

import           ClassyPrelude
import           Web.Scotty
import           Web.Scotty.Trans (ActionT)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Text.Lazy (split, replace, strip)
import           Data.Char (isSpace)
import           Data.Aeson (decode)
import           Text.Pandoc.Options
import           Network.HTTP.Types.Status

-- | Tries to parse a text ISO datetime into a UTCTime.
--
-- >>> parseISOTime $ pack "2013-10-17T09:42:49.007Z"
-- Just 2013-10-17 09:42:49.007 UTC
--
-- >>> parseISOTime $ pack "yolo"
-- Nothing
parseISOTime :: LText -> Maybe UTCTime
parseISOTime x = fmap unsafeHead $ decodeTime $ B8.pack $ unpack $ "[\"" ++ x ++ "\"]"
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
-- >>> slugify $ pack "Hello & World!"
-- "hello-and-world"
slugify :: LText -> LText
slugify = filter (not . isSpace) . intercalate "-" . words .
          replace "&" "and" . replace "+" "plus" . replace "%" "percent" .
          replace "<" "lt" . replace ">" "gt" . replace "=" "eq" .
          replace "#" "hash" . replace "@" "at" . replace "$" "dollar" .
          filter (`notElem` ("!^*?()[]{}`./\\'\"~|" :: String)) .
          toLower . strip

-- | Parses comma-separated tags into a list.
--
-- >>> parseTags $ pack "article,note, first post"
-- ["article","note","first post"]
parseTags :: LText -> [LText]
parseTags = split (== ',') . replace ", " ","

-- | Trims characters before HTML begins and after it ends
--
-- >>> dropNonHtml "something<!DOCTYPE html>...</html>something"
-- "<!DOCTYPE html>...</html>"
dropNonHtml :: (IsSequence s, Element s ~ Char) => s -> s
dropNonHtml = dropWhile (/= '<') . reverse . dropWhile (/= '>') . reverse

-- | Makes a URL from a hostname and parts
--
-- >>> mkUrl "http://localhost:4200" ["yolo", "lol"]
-- "http://localhost:4200/yolo/lol"
mkUrl :: (IsString s, Monoid s) => s -> [s] -> s
mkUrl base parts = intercalate "/" $ [base] ++ parts

type SweetrollAction = ActionT LText IO

-- | Returns an action that shows text with a Content-Type of application/x-www-form-urlencoded.
showXForm :: LText -> SweetrollAction ()
showXForm x = status ok200 >> setHeader "Content-Type" "application/x-www-form-urlencoded; charset=utf-8" >> text x

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def { readerExtensions = githubMarkdownExtensions
                          , readerSmart = True }

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def { writerHtml5 = True
                          , writerEmailObfuscation = NoObfuscation
                          , writerHighlight = True
                          , writerIdentifierPrefix = "sr-" }
