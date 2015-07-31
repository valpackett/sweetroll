{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}

module Sweetroll.Micropub (
  postMicropub
) where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork, threadDelay)
import           Data.Default
import           Data.Microformats2
import           Data.Microformats2.Parser
import           Data.Microformats2.Aeson ()
import qualified Data.Stringable as S
import           Text.Pandoc hiding (Link)
import qualified Text.Pandoc.Error as PE
import           Network.HTTP.Client
import qualified Network.HTTP.Types as HT
import           Servant
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Auth
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Syndication
import           Sweetroll.Webmention

postMicropub ∷ JWT VerifiedJWT → [(Text, Text)] → Sweetroll (Headers '[Header "Location" Text] [(Text, Text)])
postMicropub _ allParams = do
  now ← liftIO getCurrentTime
  isTest ← getConfOpt testMode
  base ← getConfOpt baseUrl
  let category = decideCategory allParams
      slug = decideSlug allParams now
      readerF = pandocRead $ decideReader allParams
      absUrl = mkUrl base $ map pack [category, slug]
      create x = liftIO $ transaction "./" $ saveNextDocument category slug x
      update x = liftIO $ transaction "./" $ saveDocumentByName category slug x
  case lookup "h" allParams of
    Just "entry" → do
      let entry' = makeEntry allParams now (S.toLazyText absUrl) readerF
      entry ← fetchCiteContexts entry'
      create $ entry { entryContent = entryContent entry ++ (join $ map derefEntryContent $ entryRepostOf entry) }
      let ifSyndicateTo x y = if any (isInfixOf x . snd) $ filter (isInfixOf "syndicate-to" . fst) allParams then y else return Nothing
      unless isTest $ do
        void $ fork $ do
          threadDelay =<< return . (*1000000) =<< getConfOpt pushDelay
          notifyPuSH []
          notifyPuSH [pack category]
        void $ fork $ do
          synd ← sequence [ ifSyndicateTo "app.net"     $ postAppDotNet entry
                          , ifSyndicateTo "twitter.com" $ postTwitter entry ]
          update $ entry { entrySyndication = catMaybes synd }
        void $ fork $ void $ sendWebmentions entry
      return $ addHeader absUrl $ []
    _ → throwError err400

decideCategory ∷ [(Text, Text)] → CategoryName
decideCategory pars | hasPar "name"          = "articles"
                    | hasPar "in-reply-to"   = "replies"
                    | hasPar "like-of"       = "likes"
                    | otherwise              = "notes"
  where hasPar = isJust . (flip lookup) pars

decideSlug ∷ [(Text, Text)] → UTCTime → EntrySlug
decideSlug pars now = unpack . fromMaybe fallback $ lookup "slug" pars
  where fallback = slugify . fromMaybe (formatTimeSlug now) $ lookupFirst ["name", "summary"] pars
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

decideReader ∷ [(Text, Text)] → (ReaderOptions → String → Either PE.PandocError Pandoc)
decideReader pars | f == "textile"     = readTextile
                  | f == "org"         = readOrg
                  | f == "rst"         = readRST
                  | f == "html"        = readHtml
                  | f == "latex"       = readLaTeX
                  | f == "tex"         = readLaTeX
                  | otherwise          = readMarkdown
  where f = orEmptyMaybe $ lookup "format" pars

makeEntry ∷ [(Text, Text)] → UTCTime → LText → (String → Pandoc) → Entry
makeEntry pars now absUrl readerF = def
  { entryName         = par "name"
  , entrySummary      = par "summary"
  , entryContent      = PandocContent <$> readerF <$> unpack <$> par "content"
  , entryPublished    = pure . fromMaybe now . headMay . catMaybes $ parseISOTime <$> par "published"
  , entryUpdated      = pure now
  , entryAuthor       = TextCard <$> par "author"
  , entryCategory     = filter (not . null) . join $ parseTags <$> par "category"
  , entryUrl          = pure absUrl
  , entryInReplyTo    = UrlEntry <$> par "in-reply-to"
  , entryLikeOf       = UrlEntry <$> par "like-of"
  , entryRepostOf     = UrlEntry <$> par "repost-of" }
  where par = map S.toLazyText . maybeToList . (flip lookup) pars

fetchCiteContexts ∷ Entry → Sweetroll Entry
fetchCiteContexts e = do
  let fetchCite (UrlEntry href) = do
        resp ← parseReprEntryWithAuthor (requestGetHtml . show) Sanitize href
        return $ Just $ case resp of
          Just entry → CiteEntry $ removeSameName $ citeOfEntry entry { entryUrl = [href] }
          _ → UrlEntry href
      fetchCite x = return $ Just x
  inReplyTo ← sequence $ fetchCite <$> entryInReplyTo e
  likeOf ← sequence $ fetchCite <$> entryLikeOf e
  repostOf ← sequence $ fetchCite <$> entryRepostOf e
  return $ e { entryInReplyTo = catMaybes inReplyTo
             , entryLikeOf    = catMaybes likeOf
             , entryRepostOf  = catMaybes repostOf }

notifyPuSH ∷ [Text] → Sweetroll ()
notifyPuSH url = do
  base ← getConfOpt baseUrl
  req ← parseUrlP "" =<< getConfOpt pushHub
  resp ← request $ req { method = "POST"
                       , requestHeaders = [ (HT.hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                       , requestBody = RequestBodyBS . writeForm $ [ (asText "hub.mode", asText "publish"), ("hub.url", mkUrl base url) ]
                       } ∷ Sweetroll (Response LByteString)
  putStrLn $ "PubSubHubbub status: " ++ (S.toText . show . HT.statusCode . responseStatus $ resp)
