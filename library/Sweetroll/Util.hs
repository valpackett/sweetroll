{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE GADTs, FlexibleContexts, QuasiQuotes #-}

-- | Various functions used inside Sweetroll.
module Sweetroll.Util where

import           ClassyPrelude hiding (fromString, headMay)
import           Control.Error.Util (hush)
import           Data.Text (replace, strip)
import           Data.Char (isSpace)
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import           Data.Proxy
import           Text.Regex.PCRE.Heavy
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Error as PE
import           Safe (headMay)
import           Network.Wai (Middleware, responseLBS, pathInfo)
import           Network.Mime (defaultMimeLookup)
import           Network.HTTP.Types.Status (ok200)
import           Servant (mimeRender, mimeUnrender, FormUrlEncoded)
import           Sweetroll.Conf (pandocReaderOptions)

type CategoryName = String
type EntrySlug = String

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
-- >>> :set -XOverloadedStrings
-- >>> slugify "Hello & World!"
-- "hello-and-world"
slugify ∷ Text → Text
slugify = filter (not . isSpace) . intercalate "-" . words .
          replace "&" "and"  . replace "+" "plus" . replace "%" "percent" .
          replace "<" "lt"   . replace ">" "gt"   . replace "=" "eq" .
          replace "#" "hash" . replace "@" "at"   . replace "$" "dollar" .
          filter (`onotElem` ("!^*?()[]{}`./\\'\"~|" ∷ String)) .
          toLower . strip

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
parseTags ∷ Text → [Text]
parseTags ts = mapMaybe (headMay . snd) $ scan r ts
  where r = [re|([^,]+),?\s?|]

-- | Encodes key-value data as application/x-www-form-urlencoded.
writeForm ∷ (ConvertibleStrings α Text, ConvertibleStrings β Text, ConvertibleStrings LByteString γ) ⇒ [(α, β)] → γ
writeForm = fromLBS . mimeRender (Proxy ∷ Proxy FormUrlEncoded) . map (toST *** toST)

-- | Decodes key-value data from application/x-www-form-urlencoded.
readForm ∷ (ConvertibleStrings Text α , ConvertibleStrings Text β, ConvertibleStrings γ LByteString) ⇒ γ → Maybe [(α, β)]
readForm x = map (fromST *** fromST) <$> hush (mimeUnrender (Proxy ∷ Proxy FormUrlEncoded) $ toLBS x)

orEmptyMaybe ∷ IsString α ⇒ Maybe α → α
orEmptyMaybe = fromMaybe ""

pandocRead ∷ (P.ReaderOptions → String → Either PE.PandocError P.Pandoc) → String → P.Pandoc
pandocRead x = PE.handleError . x pandocReaderOptions

serveStaticFromLookup ∷ [(FilePath, ByteString)] → Middleware
serveStaticFromLookup files app req respond =
  case lookup pth files of
    Just bs → respond $ responseLBS ok200 [("Content-Type", ctype)] $ cs bs
    Nothing → app req respond
  where pth = cs $ intercalate "/" reqpath
        ctype = defaultMimeLookup $ orEmptyMaybe $ lastMay reqpath
        reqpath = drop 1 $ pathInfo req
