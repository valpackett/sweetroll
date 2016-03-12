{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Sweetroll.Micropub.Response where

import           Sweetroll.Prelude
import qualified Data.Map as M
import           Servant

data MicropubResponse = Posted
                      | SyndicateTo [Text]
                      | AuthInfo [(Text, Text)]
                      | Source Value

instance ToJSON MicropubResponse where
  toJSON Posted = toJSON $ object [ ]
  toJSON (SyndicateTo urls) = toJSON $ object [ "syndicate-to" .= urls ]
  toJSON (AuthInfo params) = toJSON $ M.fromList params
  toJSON (Source val) = val

instance ToFormUrlEncoded MicropubResponse where
  toFormUrlEncoded Posted = []
  toFormUrlEncoded (SyndicateTo urls) = map ("syndicate-to[]", ) urls
  toFormUrlEncoded (AuthInfo params) = params
  toFormUrlEncoded (Source val) = [] -- XXX: should just ignore Accept for source? make it a separate route?
