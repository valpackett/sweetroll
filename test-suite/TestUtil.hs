{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

module TestUtil where

import           ClassyPrelude
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import qualified Data.Stringable as S
import           Data.Default (def)
import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Network.Wai
import           Network.Wai.Test
import           Network.HTTP.Types
import           Sweetroll.Conf (secretKey)
import           Sweetroll.Auth (signAccessToken)
import           Sweetroll.Util (orEmptyMaybe)

contains ∷ EqSequence α ⇒ α → α → Bool
contains = flip isInfixOf

get' ∷ Request → ByteString → Application → IO SResponse
get' r = runSession . request . setPath r { requestMethod = renderStdMethod GET }

get ∷ ByteString → Application → IO SResponse
get = get' defaultRequest

-- XXX: the request function uses L.empty as the body, regardless of
-- requestBody :-(

post' ∷ Request → ByteString → LByteString → Application → IO SResponse
post' r u b = runSession $ srequest $ SRequest (setPath r' u) b
  where r' = r { requestMethod = renderStdMethod POST }

post ∷ ByteString → LByteString → Application → IO SResponse
post = post' defaultRequest

postAuthed ∷ Request → ByteString → LByteString → Application → IO SResponse
postAuthed r u b a = do
  now ← getCurrentTime
  let token = signAccessToken (secretKey def) "localhost" "me" now
      r' = r { requestHeaders = requestHeaders r ++ [("Authorization", "Bearer " ++ (S.toByteString token))] }
  post' r' u b a

header ∷ SResponse → String → String
header resp x = B8.unpack $ orEmptyMaybe $ lookup (CI.mk $ B8.pack x) (simpleHeaders resp)

mf2o ∷ [Pair] → Value
mf2o ps = object [ "type" .= [ asText "h-entry" ], "properties" .= object ps ]
