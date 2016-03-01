{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, QuasiQuotes #-}

module Sweetroll.Webmention.Receive where

import           ClassyPrelude
import           Control.Lens hiding (snoc)
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork)
import qualified Data.HashMap.Strict as HMS
import           Data.String.Conversions
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.QQ
import           Network.URI
import           Network.HTTP.Types
import           Network.HTTP.Client.Conduit
import           Servant
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Monads

receiveWebmention ∷ [(Text, Text)] → Sweetroll ()
receiveWebmention allParams = do
  source ← guardJustP (errNoURIInField "source") $ parseURI =<< cs <$> lookup "source" allParams
  target ← guardJustP (errNoURIInField "target") $ parseURI =<< cs <$> lookup "target" allParams
  (category, slug) ← parseEntryURI target
  void $ guardJust errWrongPath $ documentIdFromName category slug
  shouldBeSync ← getConfOpt testMode
  (if shouldBeSync then void else void . fork) $ processWebmention category slug source target
  throwError respAccepted

processWebmention ∷ String → String → URI → URI → Sweetroll ()
processWebmention category slug source target =
  void $ withRequestHtml source $ \resp → do
    let withEntry x =
          transaction "." $ do
            entrym ← readDocumentByName category slug
            case entrym of
              Just entry → x entry
              _ → putStrLn $ "Received webmention for nonexistent " ++ tshow target ++ " from " ++ tshow source
    case statusCode $ responseStatus resp of
      410 → withEntry $ \entry → do
              putStrLn $ "Received gone webmention for " ++ tshow target ++ " from " ++ tshow source
              let updatedEntry = upsertMention (entry ∷ Value) tombstone
              saveDocumentByName category slug updatedEntry
      200 → void $ withFetchEntryWithAuthors source resp $ \_ (mention, _) →
              if verifyMention target mention
                 then withEntry $ \entry → do
                   putStrLn $ "Received correct webmention for " ++ tshow target ++ " from " ++ tshow source
                   let updatedEntry = upsertMention (entry ∷ Value) $ ensurePresentUrl source mention
                   saveDocumentByName category slug updatedEntry
                 else putStrLn $ "Received unverified webmention for " ++ tshow target ++ " from " ++ tshow source ++ ": " ++ tshow mention
      _ → return ()
    return $ Just ()

verifyMention ∷ URI → Value → Bool
verifyMention t m | propIncludesURI t "in-reply-to" m = True
verifyMention t m | propIncludesURI t "like-of"     m = True
verifyMention t m | propIncludesURI t "repost-of"   m = True
-- TODO: check content (if we're going to support mentions that aren't replies, etc.)
verifyMention _ _ = False

propIncludesURI ∷ URI → Text → Value → Bool
propIncludesURI t p m = elem t $ catMaybes $ map (parseURI <=< unCite) $ fromMaybe empty $ m ^? key "properties" . key p . _Array
  where unCite v@(Object _) = cs <$> v ^? key "value" . _String
        unCite (String s)   = Just $ cs s
        unCite _            = Nothing

upsertMention ∷ Value → Value → Value
upsertMention root mention = if replaced
                                then root'
                                else if isJust $ root' ^? responses
                                  then root' & responses . _Array %~ (flip snoc mention)
                                  else root' & key "properties" . _Object %~ (HMS.insert "comment" (Array $ singleton mention))
  where (replaced, root') = dfReplace False root
        -- depth-first replacement, flag is True when replacement was already performed
        dfReplace flag x =
            let (flag', rsps) = foldl' step (flag, []) (x ^.. responses . values)
                x' = x & responses .~ Array (reverse $ fromList rsps) in
                if flag'
                   then (True, x')
                   else if intersectingUrls (urls mention) x
                     then (True, mention)
                     else (False, x')
        step (True, acc) el = (True, el : acc)
        step (False, acc) el = let (rflag, rel) = dfReplace False el in (rflag, rel : acc)
        responses = key "properties" . key "comment"

intersectingUrls ∷ Vector Value → Value → Bool
intersectingUrls us e = hasIntersection us (urls e)
  where hasIntersection x = not . null . filter (`elem` x)

urls ∷ Value → Vector Value
urls x = fromMaybe empty $ x ^? key "properties" . key "url" . _Array

ensurePresentUrl ∷ URI → Value → Value
ensurePresentUrl source mention = mention & key "properties" . key "url" . _Array %~ ensure
  where ensure v | length v > 0 = v
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
