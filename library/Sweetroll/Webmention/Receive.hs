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
-- TODO: verify that it links to target

receiveWebmention ∷ [(Text, Text)] → Sweetroll ()
receiveWebmention allParams = do
  source ← guardJustP (errNoURIInField "source") $ parseURI =<< cs <$> lookup "source" allParams
  target ← guardJustP (errNoURIInField "target") $ parseURI =<< cs <$> lookup "target" allParams
  base ← getConfOpt baseURI
  guardBool errWrongDomain $ (uriRegName <$> uriAuthority target) == (uriRegName <$> uriAuthority base)
  void $ case map cs $ splitOn "/" $ drop 1 $ cs $ uriPath target of
    (category : slug : []) → do
      void $ guardJust errWrongPath $ documentIdFromName category slug
      void $ fork $ processWebmention category slug source
      throwError respAccepted
    _ → throwError errWrongPath
  return ()

processWebmention ∷ String → String → URI → Sweetroll ()
processWebmention category slug source =
  void $ withSuccessfulRequestHtml source $ \resp →
  withFetchEntryWithAuthors source resp $ \_ (mfE, _) →
    transaction "." $ do
      entrym ← readDocumentByName category slug
      case entrym of
        Just entry → do
          let addResponse (Object ps) = Object $ HMS.insertWith concVal "comment" (Array $ V.singleton mfE) ps
              addResponse x = x
              concVal (Array new) (Array old) = Array $ new ++ old
              concVal anew@(Array _) _ = anew
              concVal _ old = old
          saveDocumentByName category slug $ key "properties" %~ addResponse $ (entry ∷ Value)
        _ → return ()

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
