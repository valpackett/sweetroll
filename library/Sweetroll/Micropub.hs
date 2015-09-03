{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}

module Sweetroll.Micropub (
  postMicropub
) where

import           ClassyPrelude
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork, threadDelay)
import           Control.Monad.Trans.Control
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.Microformats2.Parser.Util (emptyVal)
import           Data.String.Conversions
import qualified Data.Vector as V
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
import           Sweetroll.Webmention.Send

postMicropub ∷ JWT VerifiedJWT → [(Text, Text)] → Sweetroll (Headers '[Header "Location" Text] [(Text, Text)])
postMicropub _ allParams = do
  case lookup "h" allParams of
    Just "entry" → do

      -- ## Reply contexts
      let paramToEntries x = forM (mapMaybe parseURI $ map cs $ maybeToList $ lookup x allParams) $ \uri →
            withSuccessfulRequestHtml uri $ \resp → 
            withFetchEntryWithAuthors uri resp $ \mfRoot (mfE, _) →
              return (mfE, fmap (uri, ) $ listToMaybe $ discoverWebmentionEndpoints mfRoot (linksFromHeader resp))
      inReplyTo ← paramToEntries "in-reply-to"
      likeOf ←    paramToEntries "like-of"
      repostOf ←  paramToEntries "repost-of"
      let replyContexts = [ "in-reply-to" .= map fst (catMaybes inReplyTo)
                          , "like-of"     .= map fst (catMaybes likeOf)
                          , "repost-of"   .= map fst (catMaybes repostOf) ]

      -- ## Webmention-to-Syndication
      (MkSyndicationConfig syndConf) ← getConfOpt syndicationConfig
      let inSyndicateTo x = any (isInfixOf x . snd) $ filter (isInfixOf "syndicate-to" . fst) allParams
          syndicationLinks = case syndConf of
                               Object o → concat $ mapMaybe (^? _String) $ mapMaybe ((flip lookup) o) $ filter inSyndicateTo $ keys o
                               _ → ""

      -- ## Basic data for the entry
      now ← liftIO getCurrentTime
      base ← getConfOpt baseURI
      let category = decideCategory allParams
          slug = decideSlug allParams now
          content = pandocRead (decideReader allParams) <$> cs <$> lookup "content" allParams
          absUrl = permalink (Proxy ∷ Proxy EntryRoute) category slug `relativeTo` base

      -- ## Creating the entry
      let entry = makeEntry allParams now absUrl content replyContexts syndicationLinks
      transaction "./" $ saveNextDocument category slug entry
      -- TODO: copy content for reposts

      -- ## Async notification jobs
      isTest ← getConfOpt testMode
      unless isTest $ void $ fork $ do
        threadDelay =<< return . (*1000000) =<< getConfOpt pushDelay
        transaction "./" $ do
          -- check that it wasn't deleted after the delay
          entry'm ← readDocumentByName category slug
          case entry'm of
            Just entry' → do
              notifyPuSH $ permalink (Proxy ∷ Proxy IndexRoute)
              notifyPuSH $ permalink (Proxy ∷ Proxy CatRouteE) category
              saveDocumentByName category slug =<< syndicate entry' absUrl syndicationLinks
              contMs ← contentWebmentions content
              let metaMs = catMaybes $ map snd $ concat $ map catMaybes [ inReplyTo, likeOf, repostOf ]
              void $ sendWebmentions absUrl (metaMs ++ contMs)
            _ → return ()
      return $ addHeader (tshow absUrl) []
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

makeEntry ∷ [(Text, Text)] → UTCTime → URI → Maybe Pandoc → [Pair] → Text → Value
makeEntry pars now absUrl content replyContexts syndicationLinks =
  object [ "type"       .= [ asText "h-entry" ]
         , "properties" .= object (("syndication" .= ([ ] ∷ [Value])) : filter (not . emptyVal . snd) props) ]
  where par = map cs . maybeToList . (flip lookup) pars
        props = [ "name"        .= par "name"
                , "summary"     .= par "summary"
                , "content"     .= case content of
                                     Just c → Array $ V.fromList [ object [ "html" .= (renderHtml (writeHtml pandocWriterOptions c) ++ cs syndicationLinks) ] ]
                                     Nothing → Array V.empty
                , "published"   .= [ fromMaybe (toJSON now) $ headMay $ toJSON <$> par "published" ]
                , "updated"     .= [ now ]
                , "author"      .= par "author"
                , "category"    .= filter (not . null) (join $ parseTags <$> par "category")
                , "url"         .= [ tshow absUrl ] ] ++ replyContexts

notifyPuSH ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
             URI → μ ()
notifyPuSH l = do
  hub ← getConfOpt pushHub
  case parseURI hub of
    Nothing → return ()
    Just hubURI → do
      base ← getConfOpt baseURI
      let pingURI = l `relativeTo` base
          body = writeForm [ (asText "hub.mode", asText "publish"), ("hub.url", tshow pingURI) ]
      let req = def { method = "POST"
                    , requestHeaders = [ (HT.hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
                    , requestBody = RequestBodyBS body }
      req' ← setUri req hubURI
      void $ withSuccessfulRequest req' $ \_ → do
        putStrLn $ "PubSubHubbub notified: " ++ cs body
        return $ Just ()

syndicate ∷ (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ) ⇒
            Value → URI → Text → μ Value
syndicate entry absUrl syndicationLinks = do
  syndMs ← contentWebmentions $ Just $ pandocRead readHtml $ cs syndicationLinks
  syndResults ← liftM catMaybes $ sendWebmentions absUrl syndMs
  let processSynd resp = do
        guard $ responseStatus resp `elem` [ HT.ok200, HT.created201 ]
        v ← decode (responseBody resp) ∷ Maybe Value
        v ^? key "url" . _String
  return $ set (key "properties" . key "syndication") (Array $ V.fromList $ map String $ mapMaybe processSynd syndResults) entry
