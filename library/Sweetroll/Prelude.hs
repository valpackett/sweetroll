{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, QuasiQuotes #-}

-- | The Sweetroll prelude == ClassyPrelude + more stuff.
module Sweetroll.Prelude (
  module ClassyPrelude
, module Control.Error.Util
, module Control.Monad.Except
, module Control.Monad.Trans.Control
, module Control.Monad.Trans.Either
, module Control.Lens
, module Data.Aeson
, module Data.Aeson.Lens
, module Data.Default
, module Data.List
, module Data.Char
, module Data.String.Conversions
, module Data.String.Conversions.Monomorphic
, module Data.Proxy
, module Network.URI
, module Network.HTTP.Types
, module Sweetroll.Prelude
) where

import           ClassyPrelude hiding (fromString)
import           Control.Error.Util (hush, note)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Either
import           Control.Lens hiding (Index, index, cons, snoc, uncons, unsnoc, (<.>), (.=), (|>))
import           Text.XML (Document, Element)
import           Data.Default
import           Data.Text (replace, strip)
import           Data.List (nub)
import           Data.Char (isSpace)
import           Data.String.Conversions hiding ((<>))
import           Data.String.Conversions.Monomorphic
import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text (splitOn)
import           Network.URI
import           Network.HTTP.Types
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Error as PE
import           Servant -- (mimeRender, mimeUnrender, FormUrlEncoded)

type XDocument = Text.XML.Document
type XElement = Text.XML.Element

type CategoryName = String
type EntrySlug = String

infixl 1 |>
(|>) ∷ Monad μ ⇒ μ α → (α → β) → μ β
(|>) = flip liftM

firstStr v l = (v ^? l . _String) <|> (v ^? values . l . _String) <|> (v ^? l . values . _String)

uriPathParts ∷ ConvertibleStrings Text α ⇒ URI → [α]
uriPathParts = map cs . splitOn "/" . drop 1 . cs . uriPath

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
          filter (`onotElem` asString "!^*?()[]{}`./\\'\"~|") .
          toLower . strip

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

pandocReaderOptions ∷ P.ReaderOptions
pandocReaderOptions = def { P.readerExtensions = P.githubMarkdownExtensions
                          , P.readerSmart = True }

parseEntryURIRelative ∷ (MonadError ServantErr μ) ⇒ URI → μ (String, String)
parseEntryURIRelative uri =
  case uriPathParts uri of
    [ category, slug ] → return (category, slug)
    _ → throwError errWrongPath

errWrongDomain ∷ ServantErr
errWrongDomain = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                        , errBody    = "The target URI is not on this domain." }

errWrongPath ∷ ServantErr
errWrongPath = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                      , errBody    = "The target URI is not a resource that exists on this domain." }

errNoURIInField ∷ LByteString → ServantErr
errNoURIInField f = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                           , errBody    = "You didn't put a valid absolute URI in the '" ++ f ++ "' field of the www-form-urlencoded request body." }

guardJustP ∷ MonadError ServantErr μ ⇒ ServantErr → Maybe α → μ α
guardJustP _ (Just x) = return x
guardJustP e Nothing = throwError e

guardJustM ∷ MonadError ServantErr μ ⇒ μ ServantErr → μ (Maybe α) → μ α
guardJustM ea a = a >>= \x → case x of
                                 Just v → return v
                                 Nothing → throwError =<< ea

guardJust ∷ MonadError ServantErr μ ⇒ ServantErr → μ (Maybe α) → μ α
guardJust e = guardJustM (return e)

guardBool ∷ MonadError ServantErr μ ⇒ ServantErr → Bool → μ ()
guardBool e x = unless x $ throwError e

guardBoolM ∷ MonadError ServantErr μ ⇒ μ ServantErr → Bool → μ ()
guardBoolM ea False = throwError =<< ea
guardBoolM _ True = return ()
