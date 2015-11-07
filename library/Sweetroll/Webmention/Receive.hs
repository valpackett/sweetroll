{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Sweetroll.Webmention.Receive where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Concurrent.Lifted (fork)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import           Data.String.Conversions
import           Data.Text (splitOn)
import           Data.Aeson
import           Data.Aeson.Lens
import           Network.URI
import           Network.HTTP.Types
import           Servant
import           Gitson
import           Sweetroll.Conf
import           Sweetroll.Monads

-- TODO: rate limiting
-- TODO: status viewing
-- TODO: update existing comments

receiveWebmention ∷ [(Text, Text)] → Sweetroll ()
receiveWebmention allParams = do
  source ← guardJustP (errNoURIInField "source") $ parseURI =<< cs <$> lookup "source" allParams
  target ← guardJustP (errNoURIInField "target") $ parseURI =<< cs <$> lookup "target" allParams
  base ← getConfOpt baseURI
  shouldBeSync ← getConfOpt testMode
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority target) == (uriRegName <$> uriAuthority base)
  void $ case map cs $ splitOn "/" $ drop 1 $ cs $ uriPath target of
    [ category, slug ] → do
      void $ guardJust errWrongPath $ documentIdFromName category slug
      (if shouldBeSync then void else void . fork) $ processWebmention category slug source target
      throwError respAccepted
    _ → throwError errWrongPath
  return ()

processWebmention ∷ String → String → URI → URI → Sweetroll ()
processWebmention category slug source target =
  void $ withSuccessfulRequestHtml source $ \resp →
  withFetchEntryWithAuthors source resp $ \_ (mention, _) →
  transaction "." $ do
    entrym ← readDocumentByName category slug
    case (entrym, verifyMention target mention) of
      (Just entry, True) → do
        putStrLn $ "Received valid webmention for " ++ tshow target ++ " from " ++ tshow source
        let updatedEntry = key "properties" %~ addMention mention $ (entry ∷ Value)
        saveDocumentByName category slug updatedEntry
      _ → putStrLn $ "Received invalid webmention for " ++ tshow target ++ " from " ++ tshow source ++ ": " ++ tshow mention

verifyMention ∷ URI → Value → Bool
verifyMention t m | propIncludesURI t "in-reply-to" m = True
verifyMention t m | propIncludesURI t "like-of"     m = True
verifyMention t m | propIncludesURI t "repost-of"   m = True
-- TODO: check content
verifyMention _ _ = False

propIncludesURI ∷ URI → Text → Value → Bool
propIncludesURI t p m = elem t $ catMaybes $ map (parseURI <=< unCite) $ fromMaybe V.empty $ m ^? key "properties" . key p . _Array
  where unCite v@(Object _) = cs <$> v ^? key "value" . _String
        unCite (String s)   = Just $ cs s
        unCite _            = Nothing

addMention ∷ Value → Value → Value
addMention m (Object props) = Object $ HMS.insertWith updateOrAdd "comment" (Array $ V.singleton m) props
  where updateOrAdd (Array new) (Array old) = Array $ old ++ new
        updateOrAdd anew@(Array _) _ = anew
        updateOrAdd _ old = old
addMention _ x = x

errNoURIInField ∷ LByteString → ServantErr
errNoURIInField f = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                           , errBody    = "You didn't put a valid absolute URI in the '" ++ f ++ "' field of the www-form-urlencoded request body." }

errWrongDomain ∷ ServantErr
errWrongDomain = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                        , errBody    = "The target URI is not on this domain." }

errWrongPath ∷ ServantErr
errWrongPath = err400 { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                      , errBody    = "The target URI is not a resource that exists on this domain." }

respAccepted ∷ ServantErr -- XXX: Only way to return custom HTTP response codes
respAccepted = ServantErr { errHTTPCode = 202
                          , errReasonPhrase = "Accepted"
                          , errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                          , errBody    = "Your webmention will be processed soon." }
