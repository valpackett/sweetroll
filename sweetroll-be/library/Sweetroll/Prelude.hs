{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

-- | The Sweetroll prelude == ClassyPrelude + more stuff.
module Sweetroll.Prelude (
  module X
, module Sweetroll.Prelude
) where

import           ClassyPrelude as X hiding (fromString)
import           Control.Error.Util as X hiding (hoistEither, (??), tryIO, bool)
import           Control.Monad.Except as X (MonadError, throwError)
import           Control.Monad.Trans.Except as X
import           Control.Monad.Trans.Control as X
import           Control.Monad.Trans.Either as X
import           Control.Monad.Trans.Maybe as X hiding (liftListen, liftPass, liftCallCC)
import           Control.Lens as X hiding (Index, index, cons, snoc, uncons, unsnoc, (<.>), (.=), (|>), (<|))
import           Text.XML (Document, Element)
import           Data.Default as X
import qualified Data.Text
import           Data.Text (replace, strip)
import           Data.Maybe (fromJust)
import           Data.Has as X
import           Data.List as X (nub)
import           Data.List.Split as X (splitOn)
import           Data.Char as X (isSpace, generalCategory, GeneralCategory(..))
import           Data.String.Conversions as X hiding ((<>))
import           Data.String.Conversions.Monomorphic as X
import qualified Data.HashMap.Strict as HMS
import           Data.Proxy as X
import           Data.Aeson as X
import           Data.Aeson.Lens as X hiding (nonNull)
import           Network.URI as X
import           Network.HTTP.Types as X
import           Servant -- (mimeRender, mimeUnrender, FormUrlEncoded)

type XDocument = Text.XML.Document
type XElement = Text.XML.Element

infixl 1 |>
(|>) ∷ Monad μ ⇒ μ α → (α → β) → μ β
(|>) = flip liftM

firstStr v l = (v ^? l . _String) <|> (v ^? values . l . _String) <|> (v ^? l . values . _String)

uriPathParts ∷ ConvertibleStrings Text α ⇒ URI → [α]
uriPathParts = map cs . Data.Text.splitOn "/" . drop 1 . cs . uriPath

mergeVal ∷ Value → Value → Value
mergeVal (Object x) (Object y) = Object $ HMS.unionWith mergeVal x y
mergeVal x _ = x

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

ensureArrayProp ∷ Text → Value → Value
ensureArrayProp k (Object o) | HMS.member k o = Object o
ensureArrayProp k (Object o) = Object $ HMS.insert k (Array empty) o
ensureArrayProp _ v = v

errText ∷ ServantErr → LByteString → ServantErr
errText e t = e { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                , errBody    = t }

throwErrText ∷ MonadError ServantErr μ ⇒ ServantErr → LByteString → μ α
throwErrText e t = throwError $ errText e t

errNoAuth ∷ ServantErr
errNoAuth = errText err401 "Authorization/access_token not found."

errWrongAuth ∷ ServantErr
errWrongAuth = errText err401 "Invalid auth token."

errWrongDomain ∷ ServantErr
errWrongDomain = errText err400 "The target URI is not on this domain."

errWrongPath ∷ ServantErr
errWrongPath = errText err400 "The target URI is not a resource that exists on this domain."

errNoURIInField ∷ LByteString → ServantErr
errNoURIInField f = errText err400 $ "You didn't put a valid absolute URI in the '" ++ f ++ "' field of the www-form-urlencoded request body."

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

base ∷ ConvertibleStrings a String ⇒ Maybe a → URI
base (Just x) = fromJust $ parseURI $ "https://" ++ cs x -- have to parse because it might have a port number
base Nothing = URI "http:" (Just $ URIAuth "" "localhost" "") "" "" ""

parseUri ∷ ConvertibleStrings α String ⇒ α → URI
parseUri = fromJust . parseURI . cs

ensureRightDomain ∷ MonadError ServantErr μ ⇒ URI → URI → μ ()
ensureRightDomain x y = guardBool errWrongDomain $ (uriRegName <$> uriAuthority x) == (uriRegName <$> uriAuthority y)

guardEntryNotFound ∷ MonadError ServantErr μ ⇒ Maybe α → μ α
guardEntryNotFound (Just obj) = return obj
guardEntryNotFound Nothing = throwErrText err404 "Entry not found."
