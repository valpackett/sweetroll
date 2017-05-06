{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections, GADTs, RankNTypes, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}

module Sweetroll.Webmention.Receive where

import           Sweetroll.Prelude hiding (snoc, r)
import           Control.Lens (snoc)
import           Text.XML.Lens
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Vector as V
import           Sweetroll.Database
import           Sweetroll.Context
import           Sweetroll.HTTPClient hiding (Header)

receiveWebmention ∷ [(Text, Text)] → Sweetroll NoContent
receiveWebmention allParams = do
  source ← guardJustP (errNoURIInField "source") $ lookup "source" allParams
  target ← guardJustP (errNoURIInField "target") $ lookup "target" allParams
  void $ guardJustP (errNoURIInField "source") $ parseURI $ cs source
  void $ guardJustP (errNoURIInField "source") $ parseURI $ cs target
  void $ fork $ processWebmention source target
  return NoContent -- throw respAccepted

processWebmention ∷ Text → Text → Sweetroll ()
processWebmention source target = do
  let forfrom = "for " ++ tshow target ++ " from " ++ tshow source
  resp0 ← runHTTP $ reqU (parseUri source) >>= anyStatus >>= performWithHtml
  case resp0 of
    Left e →
      $logInfo$ "Error fetching webmention " ++ forfrom ++ ": " ++ e
    Right resp → do
      case statusCode $ responseStatus resp of
        410 → do
          $logInfo$ "Received gone webmention " ++ forfrom ++ tshow source
          guardDbError =<< queryDb (tshow source) deleteObject
        200 → do
          (mention0, _) ← fetchEntryWithAuthors (parseUri source) resp
          case mention0 of
            Just mention@(Object _) | verifyMention (parseUri target) mention (responseBody resp) → do
              $logInfo$ "Received correct webmention for " ++ tshow target ++ " from " ++ tshow source
              lds ← return . S.fromList . map parseUri =<< guardDbError =<< queryDb () getLocalDomains
              -- Fetch the actual stuff in the background if allowed:
              when (not $ any (parseUri source `compareDomain`) lds) $ void $ fork $ do
                obj' ← mapMOf (key "properties" . _Object) (fetchLinkedEntires lds S.empty) mention
                guardDbError =<< queryDb obj' upsertObject
              -- Insert the link into the target right now:
              void $ guardEntryNotFound =<< guardTxError =<< transactDb (do
                obj' ← queryTx target getObject
                case obj' of
                  Just obj → do
                    let updatedEntry = upsertMention (obj ∷ Value) $ ensurePresentUrl (parseUri source) mention
                    queryTx updatedEntry upsertObject
                    return $ Just obj
                  _ → return Nothing)
            Just mention@(Object _) → do
              $logInfo$ "Received unverified webmention " ++ forfrom ++ ": " ++ tshow mention
              guardDbError =<< queryDb (tshow source) deleteObject
            Just mention →
              $logInfo$ "Received incorrectly parsed webmention " ++ forfrom ++ ": " ++ tshow mention
            Nothing →
              $logInfo$ "Received unreadable webmention " ++ forfrom
        x → $logInfo$ "Received status code " ++ tshow x ++ " when fetching webmention " ++ forfrom

verifyMention ∷ URI → Value → XDocument → Bool
verifyMention t m _ | propIncludesURI t "in-reply-to"   m = True
verifyMention t m _ | propIncludesURI t "like-of"       m = True
verifyMention t m _ | propIncludesURI t "bookmark-of"   m = True
verifyMention t m _ | propIncludesURI t "repost-of"     m = True
verifyMention t m _ | propIncludesURI t "quotation-of"  m = True
verifyMention t _ b = isJust $ b ^? root . entire . named "a" . attributeIs "href" (tshow t)

propIncludesURI ∷ URI → Text → Value → Bool
propIncludesURI t p m = elem t $ catMaybes $ map (parseURI <=< unCite) $ fromMaybe empty $ m ^? key "properties" . key p . _Array
  where unCite v@(Object _) = cs <$> v ^? key "value" . _String
        unCite (String s)   = Just $ cs s
        unCite _            = Nothing

upsertMention ∷ Value → Value → Value
upsertMention obj mention =
  obj & key "properties" . _Object %~ HMS.insert "comment" (Array $ V.fromList upd)
  where orig = obj ^.. key "properties" . key "comment" . values
        upd = if any (urlMatches mention) orig
                 then orig
                 else orig ++ (String <$> (toList $ headMay $ urls mention))

urlMatches ∷ Value → Value → Bool
urlMatches (String l) (String r) = l == r
urlMatches (Object l) (String r) = any (== r) $ urls (Object l)
urlMatches (String l) (Object r) = urlMatches (Object r) (String l)
urlMatches (Object l) (Object r) = any (urlMatches (Object r)) $ map String $ urls (Object l)
urlMatches _ _ = False

urls ∷ Value → [Text]
urls (Object x) = (Object x) ^.. key "properties" . key "url" . values . _String
urls (String x) = [x]
urls _ = []

-- this is just in case the parsed entry won't have a url field at all
ensurePresentUrl ∷ URI → Value → Value
ensurePresentUrl source mention = mention & key "properties" . key "url" . _Array %~ ensure
  where ensure v | not (null v) = v
        ensure v = snoc v $ String $ tshow source

respAccepted ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respAccepted = ServantErr { errHTTPCode = 202
                          , errReasonPhrase = "Accepted"
                          , errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                          , errBody    = "Your webmention will be processed soon." }
