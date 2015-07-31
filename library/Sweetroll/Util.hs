{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE GADTs, FlexibleContexts, QuasiQuotes #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util where

import           ClassyPrelude hiding (fromString, headMay)
import           Data.Text.Lazy (replace, strip)
import           Data.Char (isSpace)
import           Data.Aeson (decode)
import           Data.Stringable hiding (length)
import           Data.Proxy
import           Data.Microformats2
import           Text.Regex.PCRE.Heavy
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Error as PE
import           Safe (headMay)
import           Network.Wai (Middleware, responseLBS, pathInfo)
import           Network.Mime (defaultMimeLookup)
import           Network.HTTP.Types.Status (ok200)
import           Servant (mimeRender, FormUrlEncoded)
import           Sweetroll.Conf (pandocReaderOptions, pandocWriterOptions)

type CategoryName = String
type EntrySlug = String

-- | Tries to parse a text ISO datetime into a UTCTime.
--
-- >>> parseISOTime "2013-10-17T09:42:49.007Z"
-- Just 2013-10-17 09:42:49.007 UTC
--
-- >>> parseISOTime "yolo"
-- Nothing
parseISOTime ∷ Stringable α ⇒ α → Maybe UTCTime
parseISOTime x = headMay =<< decodeTime ("[\"" ++ toLazyByteString x ++ "\"]")
  where decodeTime y = decode y ∷ Maybe [UTCTime]

-- | Tries to find a key-value pair by key and return the value, trying all given keys.
--
-- >>> lookupFirst ["hdd", "ram"] [("cpus", 1), ("ram", 1024)]
-- Just 1024
lookupFirst ∷ Eq α ⇒ [α] → [(α, β)] → Maybe β
lookupFirst (x:xs) ps = case lookup x ps of
  Nothing → lookupFirst xs ps
  m → m
lookupFirst [] _ = Nothing

-- | Prepares a text for inclusion in a URL.
--
-- >>> slugify "Hello & World!"
-- "hello-and-world"
slugify ∷ Stringable α ⇒ α → α
slugify = fromLazyText . filter (not . isSpace) . intercalate "-" . words .
          replace "&" "and"  . replace "+" "plus" . replace "%" "percent" .
          replace "<" "lt"   . replace ">" "gt"   . replace "=" "eq" .
          replace "#" "hash" . replace "@" "at"   . replace "$" "dollar" .
          filter (`onotElem` ("!^*?()[]{}`./\\'\"~|" ∷ String)) .
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
parseTags ∷ Stringable α ⇒ α → [α]
parseTags ts = mapMaybe (headMay . snd) $ scan r ts
  where r = [re|([^,]+),?\s?|]

-- | Makes a URL from a hostname and parts
--
-- >>> mkUrl "http://localhost:4200" ["yolo", "lol"]
-- "http://localhost:4200/yolo/lol"
mkUrl ∷ (IsString α, Monoid α) ⇒ α → [α] → α
mkUrl base parts = intercalate "/" $ base : parts

-- | Encodes key-value data as application/x-www-form-urlencoded.
writeForm ∷ (Stringable α, Stringable β, Stringable γ) ⇒ [(α, β)] → γ
writeForm = fromLazyByteString . mimeRender (Proxy ∷ Proxy FormUrlEncoded) . map (toText *** toText)

trimmedText ∷ Index LText → EntryReference → (Bool, LText)
trimmedText l e = (isArticle, if isTrimmed then take (l - 1) t ++ "…" else t)
  where isTrimmed = length t > fromIntegral l
        (isArticle, t) = case derefEntryName e of
                           Just n → (True, n)
                           _ → (False, renderContent P.writePlain e)

derefEntryUrl ∷ EntryReference → Maybe LText
derefEntryUrl (EntryEntry e) = headMay . reverse . sortOn length . entryUrl $ e
derefEntryUrl (CiteEntry c)  = headMay . reverse . sortOn length . citeUrl $ c
derefEntryUrl (TextEntry l)  = Just l
derefEntryUrl (UrlEntry l)   = Just l

derefEntryName ∷ EntryReference → Maybe LText
derefEntryName (EntryEntry e) = headMay . reverse . sortOn length . entryName $ e
derefEntryName (CiteEntry c)  = headMay . reverse . sortOn length . citeName $ c
derefEntryName (TextEntry l)  = Just l
derefEntryName (UrlEntry l)   = Just l

derefEntryContent ∷ EntryReference → [ContentReference]
derefEntryContent (EntryEntry e) = entryContent $ e
derefEntryContent (CiteEntry e)  = citeContent  $ e
derefEntryContent _              = mzero

renderContent ∷ (P.WriterOptions → P.Pandoc → String) → EntryReference → LText
renderContent writer e = case headMay $ derefEntryContent e of
  Just (PandocContent p) → pack $ writer pandocWriterOptions p
  Just (TextContent t) → t
  _ -> ""

derefEntryAuthor ∷ EntryReference → [CardReference]
derefEntryAuthor (EntryEntry e) = entryAuthor $ e
derefEntryAuthor (CiteEntry c)  = citeAuthor $ c
derefEntryAuthor _              = mzero

derefAuthorName ∷ CardReference → Maybe LText
derefAuthorName (CardCard c) = headMay . cardName $ c
derefAuthorName (TextCard t) = Just t

derefAuthorUrl ∷ CardReference → Maybe LText
derefAuthorUrl (CardCard c) = headMay . cardUrl $ c
derefAuthorUrl _            = mzero

removeSameName ∷ Cite → Cite
removeSameName c = c { citeName = filter (\x → TextContent x `onotElem` citeContent c) $ citeName c }

orEmptyMaybe ∷ IsString α ⇒ Maybe α → α
orEmptyMaybe = fromMaybe ""

orEmpty ∷ IsString α ⇒ [α] → α
orEmpty = orEmptyMaybe . headMay

pandocRead ∷ (P.ReaderOptions → String → Either PE.PandocError P.Pandoc) → String → P.Pandoc
pandocRead x = PE.handleError . x pandocReaderOptions

serveStaticFromLookup ∷ [(FilePath, ByteString)] → Middleware
serveStaticFromLookup files app req respond =
  case lookup path files of
    Just bs → respond $ responseLBS ok200 [("Content-Type", ctype)] $ toLazyByteString bs
    Nothing → app req respond
  where path = toString $ intercalate "/" reqpath
        ctype = defaultMimeLookup $ fromMaybe "" $ lastMay reqpath
        reqpath = drop 1 $ pathInfo req
