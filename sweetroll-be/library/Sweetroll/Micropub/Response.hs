{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Sweetroll.Micropub.Response where

import           Sweetroll.Prelude
import qualified Data.Map as M
import           Web.FormUrlEncoded

data MicropubResponse = Posted
                      | AuthInfo [(Text, Text)]
                      | Source Value
                      | SyndicateTo [Value]
                      | MediaEndpoint Text
                      | MultiResponse [MicropubResponse]

instance ToJSON MicropubResponse where
  toJSON Posted = toJSON $ object [ ]
  toJSON (AuthInfo params) = toJSON $ M.fromList params
  toJSON (Source val) = val
  toJSON (SyndicateTo urls) = toJSON $ object [ "syndicate-to" .= urls ]
  toJSON (MediaEndpoint url) = toJSON $ object [ "media-endpoint" .= url ]
  toJSON (MultiResponse rs) = foldl' (\a x → mergeVal a $ toJSON x) (toJSON $ object [ ]) rs

instance ToForm MicropubResponse where
  toForm Posted = toForm ([] ∷ [(Text, Text)])
  toForm (AuthInfo params) = toForm params
  toForm (Source _) = toForm ([] ∷ [(Text, Text)])
  toForm (SyndicateTo urls) = toForm $ map ((asText "syndicate-to[]", ) . fromMaybe "" . (^? key "uid" . _String)) urls
  toForm (MediaEndpoint url) = toForm [(asText "media-endpoint", url)]
  toForm (MultiResponse _) = toForm ([] ∷ [(Text, Text)]) -- Screw this.
