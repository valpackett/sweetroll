{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}

module Sweetroll.Micropub (
  postMicropub
) where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork, threadDelay)
import           Data.Aeson
import           Data.Microformats2.Parser
import           Data.Microformats2.Parser.Util (emptyVal)
import           Data.IndieWeb.MicroformatsUtil
import           Data.IndieWeb.Authorship
import qualified Data.Stringable as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Text.Pandoc hiding (Link, Null)
import qualified Text.Pandoc.Error as PE
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Network.URI (parseURI)
import           Network.HTTP.Client
import qualified Network.HTTP.Types as HT
import           Servant
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Auth
import           Sweetroll.Util
import           Sweetroll.Monads
-- import           Sweetroll.Syndication
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
      entry ← makeEntry allParams now (S.toLazyText absUrl) readerF
      -- TODO: copy content for reposts
      create entry
      -- let ifSyndicateTo x y = if any (isInfixOf x . snd) $ filter (isInfixOf "syndicate-to" . fst) allParams then y else return Nothing
      unless isTest $ do
        void $ fork $ do
          threadDelay =<< return . (*1000000) =<< getConfOpt pushDelay
          notifyPuSH []
          notifyPuSH [pack category]
        void $ fork $ do
          -- synd ← sequence [ ifSyndicateTo "app.net"     $ postAppDotNet entry
          --                 , ifSyndicateTo "twitter.com" $ postTwitter entry ]
          update $ entry -- TODO { entrySyndication = catMaybes synd }
        void $ fork $ void $ sendWebmentions entry
      return $ addHeader absUrl []
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
                  | f == "gfm"         = readMarkdown -- github flavored markdown is chosen in pandoc settings in Conf.hs
                  | otherwise          = readCommonMark
  where f = orEmptyMaybe $ lookup "format" pars

makeEntry ∷ [(Text, Text)] → UTCTime → LText → (String → Pandoc) → Sweetroll Value
makeEntry pars now absUrl readerF = do
  let par = map S.toLazyText . maybeToList . (flip lookup) pars
  inReplyTo ← mapM fetchReplyContext $ mapMaybe parseURI $ map S.toString $ par "in-reply-to"
  let props = [ "name"        .= par "name"
              , "summary"     .= par "summary"
              , "content"     .= case readerF <$> unpack <$> par "content" of
                                   c : _ → Array $ V.fromList [ object [ "html" .= renderHtml (writeHtml pandocWriterOptions c) ] ]
                                   _ → Null
              , "published"   .= [ fromMaybe now . headMay . catMaybes $ parseISOTime <$> par "published" ]
              , "updated"     .= [ now ]
              , "author"      .= par "author"
              , "category"    .= filter (not . null) (join $ parseTags <$> par "category")
              , "url"         .= [ absUrl ]
              , "in-reply-to" .= inReplyTo
              , "like-of"     .= par "like-of"
              , "repost-of"   .= par "repost-of" ]
  return $ object [ "type"       .= [ asText "h-entry" ]
                  , "properties" .= object (filter (not . emptyVal . snd) props) ]

fetchReplyContext ∷ URI → Sweetroll (Maybe Value)
fetchReplyContext uri = do
  resp' ← requestMayHtml uri
  case resp' of
    Just resp → do
      let mfRoot = parseMf2 mf2Options $ documentRoot $ parseLBS resp
      case headMay =<< allMicroformatsOfType "h-entry" mfRoot of
        Just mfEntry → do
          authors ← entryAuthors mf2Options requestMayHtml uri mfRoot mfEntry
          let addAuthors (Object o) = Object $ HMS.adjust addAuthors' "properties" o
              addAuthors x = x
              addAuthors' (Object o) = Object $ HMS.insert "author" (Array $ V.fromList $ fromMaybe [] authors) o
              addAuthors' x = x
          return $ Just $ addAuthors $ fst mfEntry
        _ → return Nothing
    _ → return Nothing

notifyPuSH ∷ [Text] → Sweetroll ()
notifyPuSH url = do
  base ← getConfOpt baseUrl
  req ← parseUrlP "" =<< getConfOpt pushHub
  resp ← request $ req { method = "POST"
                       , requestHeaders = [ (HT.hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                       , requestBody = RequestBodyBS . writeForm $ [ (asText "hub.mode", asText "publish"), ("hub.url", mkUrl base url) ]
                       } ∷ Sweetroll (Response LByteString)
  putStrLn $ "PubSubHubbub status: " ++ (S.toText . show . HT.statusCode . responseStatus $ resp)
