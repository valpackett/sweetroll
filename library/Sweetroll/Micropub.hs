{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}

module Sweetroll.Micropub (
  postMicropub
) where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork, threadDelay)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Conduit
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
import           Network.URI
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Client.Internal (setUri)
import qualified Network.HTTP.Types as HT
import           Servant
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Auth
import           Sweetroll.Util
import           Sweetroll.Monads
import           Sweetroll.Routes
import           Sweetroll.Webmention

postMicropub ∷ JWT VerifiedJWT → [(Text, Text)] → Sweetroll (Headers '[Header "Location" Text] [(Text, Text)])
postMicropub _ allParams = do
  now ← liftIO getCurrentTime
  base ← getConfOpt baseURI
  let category = decideCategory allParams
      slug = decideSlug allParams now
      content = pandocRead (decideReader allParams) <$> S.toString <$> lookup "content" allParams
      absUrl = permalink (Proxy ∷ Proxy EntryRoute) category slug `relativeTo` base
      create x = liftIO $ transaction "./" $ saveNextDocument category slug x
      -- update x = liftIO $ transaction "./" $ saveDocumentByName category slug x
  case lookup "h" allParams of
    Just "entry" → do
      let paramToEntries x = mapM fetchEntry $ mapMaybe parseURI $ map S.toString $ maybeToList $ lookup x allParams
      inReplyTo ← paramToEntries "in-reply-to"
      likeOf ←    paramToEntries "like-of"
      repostOf ←  paramToEntries "repost-of"
      let entryProps = [ "in-reply-to" .= map fst (catMaybes inReplyTo)
                       , "like-of"     .= map fst (catMaybes likeOf)
                       , "repost-of"   .= map fst (catMaybes repostOf) ]
          entry = makeEntry allParams now absUrl content entryProps
      -- TODO: copy content for reposts
      create entry
      isTest ← getConfOpt testMode
      unless isTest $ do
        void $ fork $ do
          threadDelay =<< return . (*1000000) =<< getConfOpt pushDelay
          reread ← readDocumentByName category slug ∷ Sweetroll (Maybe Value)
          if isJust reread -- not deleted after the delay
             then do
               notifyPuSH $ permalink (Proxy ∷ Proxy IndexRoute)
               notifyPuSH $ dropQueryFragment $ permalink (Proxy ∷ Proxy CatRoute) category (-1)
             else return ()
        void $ fork $ do
          contMs ← contentWebmentions content
          let metaMs = catMaybes $ map snd $ concat $ map catMaybes [ inReplyTo, likeOf, repostOf ]
          sendWebmentions absUrl (metaMs ++ contMs)
      return $ addHeader (toText $ show absUrl) []
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

makeEntry ∷ [(Text, Text)] → UTCTime → URI → Maybe Pandoc → [Pair] → Value
makeEntry pars now absUrl content entryProps = object [ "type"       .= [ asText "h-entry" ]
                                                      , "properties" .= object (filter (not . emptyVal . snd) props) ]
  where par = map S.toLazyText . maybeToList . (flip lookup) pars
        props = [ "name"        .= par "name"
                , "summary"     .= par "summary"
                , "content"     .= case content of
                                     Just c → Array $ V.fromList [ object [ "html" .= renderHtml (writeHtml pandocWriterOptions c) ] ]
                                     Nothing → Array V.empty
                , "published"   .= [ fromMaybe now . headMay . catMaybes $ parseISOTime <$> par "published" ]
                , "updated"     .= [ now ]
                , "author"      .= par "author"
                , "category"    .= filter (not . null) (join $ parseTags <$> par "category")
                , "url"         .= [ show absUrl ] ] ++ entryProps

fetchEntry ∷ URI → Sweetroll (Maybe (Value, Maybe (TargetURI, EndpointURI)))
fetchEntry uri = withSuccessfulRequestHtml uri $ \resp → do
  htmlDoc ← responseBody resp $$ sinkDoc
  let mfRoot = parseMf2 mf2Options $ documentRoot htmlDoc
  case headMay =<< allMicroformatsOfType "h-entry" mfRoot of
    Just mfEntry → do
      authors ← entryAuthors mf2Options (\u → withSuccessfulRequestHtml u $ \r → liftM Just $ responseBody r $$ sinkDoc) uri mfRoot mfEntry
      let addAuthors (Object o) = Object $ HMS.adjust addAuthors' "properties" o
          addAuthors x = x
          addAuthors' (Object o) = Object $ HMS.insert "author" (Array $ V.fromList $ fromMaybe [] authors) o
          addAuthors' x = x
      return $ Just (addAuthors $ fst mfEntry, fmap (uri, ) $ listToMaybe $ discoverWebmentionEndpoints mfRoot (linksFromHeader resp))
    _ → return Nothing

notifyPuSH ∷ URI → Sweetroll ()
notifyPuSH l = do
  hub ← getConfOpt pushHub
  case parseURI hub of
    Nothing → return ()
    Just hubURI → do
      base ← getConfOpt baseURI
      let pingURI = l `relativeTo` base
      let req = def { method = "POST"
                    , requestHeaders = [ (HT.hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                    , requestBody = RequestBodyBS . writeForm $ [ (asText "hub.mode", asText "publish"), ("hub.url", toText $ show pingURI) ] }
      req' ← setUri req hubURI
      void $ withSuccessfulRequest req' $ \_ → do
        putStrLn $ "PubSubHubbub notified for <" ++ toText (show pingURI) ++ ">"
        return $ Just ()
