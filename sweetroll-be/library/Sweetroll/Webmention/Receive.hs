{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections, GADTs, RankNTypes, FlexibleContexts, QuasiQuotes #-}

module Sweetroll.Webmention.Receive where

import           Sweetroll.Prelude hiding (snoc)
import           Control.Lens (snoc)
import qualified Data.HashMap.Strict as HMS
import           Sweetroll.Conf
import           Sweetroll.Database
import           Sweetroll.Context
import           Sweetroll.HTTPClient hiding (Header)

receiveWebmention ∷ [(Text, Text)] → Sweetroll NoContent
receiveWebmention allParams = do
  source ← guardJustP (errNoURIInField "source") $ lookup "source" allParams
  target ← guardJustP (errNoURIInField "target") $ lookup "target" allParams
  shouldBeSync ← getConfOpt testMode
  (if shouldBeSync then void else void . fork) $ processWebmention source target
  throw respAccepted

processWebmention ∷ Text → Text → Sweetroll ()
processWebmention source target = do
  let forfrom = "for " ++ tshow target ++ " from " ++ tshow source
  resp0 ← runHTTP $ reqU (parseUri source) >>= anyStatus >>= performWithHtml
  case resp0 of
    Left e → putStrLn $ "Error fetching webmention " ++ forfrom ++ ": " ++ e
    Right resp → do
      -- XXX: servant errors in a forked thread after the response...?!
      entry ← guardEntryNotFound =<< guardDbError =<< queryDb target getObject
      case statusCode $ responseStatus resp of
        410 → do
          putStrLn $ "Received gone webmention " ++ forfrom ++ tshow source
          let updatedEntry = upsertMention (entry ∷ Value) tombstone
          guardDbError =<< queryDb updatedEntry upsertObject
        200 → do
          (mention0, _) ← fetchEntryWithAuthors (parseUri source) resp
          case mention0 of
            Just mention@(Object _) | verifyMention (parseUri target) mention → do
              putStrLn $ "Received correct webmention for " ++ tshow target ++ " from " ++ tshow source
              let updatedEntry = upsertMention (entry ∷ Value) $ ensurePresentUrl (parseUri source) mention
              guardDbError =<< queryDb updatedEntry upsertObject
            Just mention@(Object _) → putStrLn $ "Received unverified webmention " ++ forfrom ++ ": " ++ tshow mention
            Just mention → putStrLn $ "Received incorrectly parsed webmention " ++ forfrom ++ ": " ++ tshow mention
            Nothing → putStrLn $ "Received unreadable webmention " ++ forfrom
        x → putStrLn $ "Received status code " ++ tshow x ++ " when fetching webmention " ++ forfrom

verifyMention ∷ URI → Value → Bool
verifyMention t m | propIncludesURI t "in-reply-to"   m = True
verifyMention t m | propIncludesURI t "like-of"       m = True
verifyMention t m | propIncludesURI t "bookmark-of"   m = True
verifyMention t m | propIncludesURI t "repost-of"     m = True
verifyMention t m | propIncludesURI t "quotation-of"  m = True
-- TODO: check content (if we're going to support mentions that aren't replies, etc.)
verifyMention _ _ = False

propIncludesURI ∷ URI → Text → Value → Bool
propIncludesURI t p m = elem t $ catMaybes $ map (parseURI <=< unCite) $ fromMaybe empty $ m ^? key "properties" . key p . _Array
  where unCite v@(Object _) = cs <$> v ^? key "value" . _String
        unCite (String s)   = Just $ cs s
        unCite _            = Nothing

upsertMention ∷ Value → Value → Value
upsertMention root mention
  | replaced = root'
  | isJust $ root' ^? key "properties" . key "comment" =
    root' & key "properties" . key "comment" . _Array %~ (`snoc` mention)
  | otherwise =
    root' & key "properties" . _Object %~ HMS.insert "comment" (Array $ singleton mention)
  where (replaced, root') = dfReplace False root
        -- depth-first replacement, flag is True when replacement was already performed
        dfReplace flag x =
            let (flag', rsps) = foldl' step (flag, []) (x ^.. key "properties" . key "comment" . values)
                x' = x & key "properties" . key "comment" .~ Array (reverse $ fromList rsps) in
                if flag'
                   then (True, x')
                   else if intersectingUrls (urls mention) x
                     then (True, mention)
                     else (False, x')
        step (True, acc) el = (True, el : acc)
        step (False, acc) el = let (rflag, rel) = dfReplace False el in (rflag, rel : acc)

intersectingUrls ∷ Vector Value → Value → Bool
intersectingUrls us e = hasIntersection us (urls e)
  where hasIntersection x = any (`elem` x)

urls ∷ Value → Vector Value
urls x = fromMaybe empty $ x ^? key "properties" . key "url" . _Array

ensurePresentUrl ∷ URI → Value → Value
ensurePresentUrl source mention = mention & key "properties" . key "url" . _Array %~ ensure
  where ensure v | not (null v) = v
        ensure v = snoc v $ String $ tshow source

respAccepted ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respAccepted = ServantErr { errHTTPCode = 202
                          , errReasonPhrase = "Accepted"
                          , errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                          , errBody    = "Your webmention will be processed soon." }

tombstone ∷ Value
tombstone = [aesonQQ|{ "properties": {
    "content": [ {"html": "This entry has been deleted."} ]
  },
  "value": "This entry has been deleted.",
  "type": ["h-entry", "h-x-tombstone"]
}|]
