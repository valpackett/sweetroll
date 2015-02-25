{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Sweetroll.Micropub (
  doMicropub
) where

import           ClassyPrelude
import           Control.Concurrent.Lifted (fork, threadDelay)
import           Data.Default
import           Data.Microformats2
import           Data.Microformats2.Aeson()
import           Data.Stringable (toText)
import           Text.Pandoc hiding (Link)
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Web.Scotty.Trans hiding (request)
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Syndication
import           Sweetroll.Webmention

doMicropub ∷ SweetrollAction ()
doMicropub = do
  h ← param "h"
  allParams ← params
  now ← liftIO getCurrentTime
  isTest ← getConfOpt testMode
  base ← getConfOpt baseUrl
  let category = decideCategory allParams
      slug = decideSlug allParams now
      readerF = decideReader allParams
      absUrl = fromStrict . mkUrl base $ map pack [category, slug]
      create x = transaction "./" $ saveNextDocument category slug x
      update x = transaction "./" $ saveDocumentByName category slug x
  case asLText h of
    "entry" → do
      let entry = makeEntry allParams now absUrl readerF
          ifSyndicateTo x y = if any (isInfixOf x . snd) $ filter (isInfixOf "syndicate-to" . fst) allParams then y else return Nothing
      create entry
      created absUrl
      unless isTest $ do
        void . fork $ do
          threadDelay =<< return . (*1000000) =<< getConfOpt pushDelay
          notifyPuSH []
          notifyPuSH [pack category]
        void . fork $ do
          let ps = [ ifSyndicateTo "app.net"     $ postAppDotNet entry
                   , ifSyndicateTo "twitter.com" $ postTwitter entry ]
          synd ← sequence ps
          let entry' = entry { entrySyndication = catMaybes synd }
          update entry'
          void . runSweetrollBase $ sendWebmentions entry'
    _ → status badRequest400

decideCategory ∷ [Param] → CategoryName
decideCategory pars | hasPar "name"          = "articles"
                    | hasPar "in-reply-to"   = "replies"
                    | hasPar "like-of"       = "likes"
                    | otherwise              = "notes"
  where hasPar = isJust . findByKey pars

decideSlug ∷ [Param] → UTCTime → EntrySlug
decideSlug pars now = unpack . fromMaybe fallback $ findByKey pars "slug"
  where fallback = slugify . fromMaybe (formatTimeSlug now) $ findFirstKey pars ["name", "summary"]
        formatTimeSlug = pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

decideReader ∷ [Param] → ReaderOptions → String → Pandoc
decideReader pars | f == "textile"     = readTextile
                  | f == "org"         = readOrg
                  | f == "rst"         = readRST
                  | f == "html"        = readHtml
                  | f == "latex"       = readLaTeX
                  | f == "tex"         = readLaTeX
                  | otherwise          = readMarkdown
  where f = fromMaybe "" $ findByKey pars "format"

makeEntry ∷ [Param] → UTCTime → LText → (ReaderOptions → String → Pandoc) → Entry
makeEntry pars now absUrl readerF = def
  { entryName         = par "name"
  , entrySummary      = par "summary"
  , entryContent      = PandocContent <$> readerF pandocReaderOptions <$> unpack <$> par "content"
  , entryPublished    = pure . fromMaybe now . headMay . catMaybes $ parseISOTime <$> par "published"
  , entryUpdated      = pure now
  , entryAuthor       = TextCard <$> par "author"
  , entryCategory     = filter (not . null) . join $ parseTags <$> par "category"
  , entryUrl          = pure absUrl
  , entryInReplyTo    = UrlEntry <$> par "in-reply-to"
  , entryLikeOf       = UrlEntry <$> par "like-of"
  , entryRepostOf     = UrlEntry <$> par "repost-of" }
  where par = maybeToList . findByKey pars

notifyPuSH ∷ [Text] → SweetrollAction ()
notifyPuSH url = do
  base ← getConfOpt baseUrl
  req ← parseUrlP "" =<< getConfOpt pushHub
  resp ← request $ req { method = "POST"
                       , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                       , requestBody = RequestBodyBS . writeForm $ [("hub.mode", "publish"), ("hub.url", mkUrl base url)]} ∷ SweetrollAction (Response LByteString)
  putStrLn $ "PubSubHubbub status: " ++ (toText . show . statusCode . responseStatus $ resp)
