{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Sweetroll.Micropub.Response where

import           Sweetroll.Prelude
import qualified Data.Map as M
import           Servant

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

instance ToFormUrlEncoded MicropubResponse where
  toFormUrlEncoded Posted = []
  toFormUrlEncoded (AuthInfo params) = params
  toFormUrlEncoded (Source val) = []
  toFormUrlEncoded (SyndicateTo urls) = map (("syndicate-to[]", ) . fromMaybe "" . (^? key "uid" . _String)) urls
  toFormUrlEncoded (MediaEndpoint url) = [("media-endpoint", url)]
  toFormUrlEncoded (MultiResponse rs) = concatMap toFormUrlEncoded rs
