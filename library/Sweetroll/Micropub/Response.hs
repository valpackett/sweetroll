{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Sweetroll.Micropub.Response where

import           Sweetroll.Prelude
import qualified Data.Map as M
import           Servant

data MicropubResponse = Posted
                      | SyndicateTo [Value]
                      | AuthInfo [(Text, Text)]
                      | Source Value

instance ToJSON MicropubResponse where
  toJSON Posted = toJSON $ object [ ]
  toJSON (SyndicateTo urls) = toJSON $ object [ "syndicate-to" .= urls ]
  toJSON (AuthInfo params) = toJSON $ M.fromList params
  toJSON (Source val) = val

instance ToFormUrlEncoded MicropubResponse where
  toFormUrlEncoded Posted = []
  toFormUrlEncoded (SyndicateTo urls) = map (("syndicate-to[]", ) . fromMaybe "" . (^? key "uid" . _String)) urls
  toFormUrlEncoded (AuthInfo params) = params
  toFormUrlEncoded (Source val) = []
