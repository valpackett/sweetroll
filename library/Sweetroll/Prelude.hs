{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}

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
import           Control.Lens as X hiding (Index, index, cons, snoc, uncons, unsnoc, (<.>), (.=), (|>))
import           Text.XML (Document, Element)
import           Text.XML.Lens
import           Data.Default as X
import           Data.Text (replace, strip, splitOn)
import           Data.List as X (nub)
import           Data.Char as X (isSpace, generalCategory, GeneralCategory(..))
import           Data.String.Conversions as X hiding ((<>))
import           Data.String.Conversions.Monomorphic as X
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Lazy as ML
import           Data.Proxy as X
import           Data.Aeson as X
import           Data.Aeson.Lens as X
import           Network.URI as X
import           Network.HTTP.Types as X
import           System.Directory
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

atomizeUri ∷ URI → URI
atomizeUri u = u { uriQuery = let q = uriQuery u in q ++ (if "?" `isPrefixOf` q then "&" else "?") ++ "_accept=application/atom%2Bxml" }

-- linksNofollow ∷ XElement → XElement
-- linksNofollow e = e & entire . el "a" . attribute "rel" %~ makeNofollow
--   where makeNofollow (Just r) = Just $ r ++ " nofollow"
--         makeNofollow Nothing  = Just "nofollow"

detwitterizeEmoji ∷ XElement → XElement
detwitterizeEmoji e = transform replaceWithAlt e
  where replaceWithAlt (Element "img" as _) | "Emoji" `isInfixOf` (fromMaybe "" $ lookup "class" as) = Element "span" (ML.fromList [("class", "emoji")]) [ NodeContent $ fromMaybe "X" $ lookup "alt" as ]
        replaceWithAlt x = x

ensureArrayProp ∷ Text → Value → Value
ensureArrayProp k (Object o) | HMS.member k o = Object o
ensureArrayProp k (Object o) = Object $ HMS.insert k (Array empty) o
ensureArrayProp _ v = v

parseEmoji ∷ (IsSequence α, X.Element α ~ Char) ⇒ α → α
parseEmoji = takeWhile isEmojiChar . dropWhile (not . isEmojiChar)
  where isEmojiChar x = generalCategory x `elem` [ Format, OtherSymbol, NonSpacingMark ]

forFileIn ∷ (MonadIO μ, MonadBaseControl IO μ) ⇒ FilePath → ([FilePath] → [FilePath]) → (FilePath → ByteString → μ ()) → μ ()
forFileIn dir fltr act = do
  exists ← liftIO $ doesDirectoryExist dir
  when exists $ do
    files ← liftIO $ getDirectoryContents dir
    forM_ (fltr files) $ \fname → do
      fcontent ← liftIO $ try $ readFile $ dir </> fname
      case fcontent of
        Right c → act fname c
        Left (e ∷ IOException) → putStrLn $ "Error when reading file " ++ cs (dir </> fname) ++ ": " ++ cs (show e)


errNoAuth ∷ ServantErr
errNoAuth = err401 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                   , errBody    = "Authorization/access_token not found." }

errWrongAuth ∷ ServantErr
errWrongAuth = err401 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                      , errBody    = "Invalid auth token." }

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
