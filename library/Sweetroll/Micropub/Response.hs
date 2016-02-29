{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Sweetroll.Micropub.Response where

import           ClassyPrelude
import           Data.Aeson
import qualified Data.Map as M
import           Servant

data MicropubResponse = Posted
                      | SyndicateTo [Text]
                      | AuthInfo [(Text, Text)]

instance ToJSON MicropubResponse where
  toJSON Posted = toJSON $ object [ ]
  toJSON (SyndicateTo urls) = toJSON $ object [ "syndicate-to" .= urls ]
  toJSON (AuthInfo params) = toJSON $ M.fromList params

instance ToFormUrlEncoded MicropubResponse where
  toFormUrlEncoded Posted = []
  toFormUrlEncoded (SyndicateTo urls) = map ("syndicate-to[]", ) urls
  toFormUrlEncoded (AuthInfo params) = params
