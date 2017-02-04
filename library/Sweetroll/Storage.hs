{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, TemplateHaskell #-}
module Sweetroll.Storage where

import           Sweetroll.Prelude hiding (Query)
import           Hasql.Query
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import           Text.RawString.QQ

getObject ∷ Query Text (Maybe Value)
getObject = statement q enc dec True
  where q = [r|SELECT jsonb_build_object('type', type, 'properties', properties, 'children', children)
               FROM objects
               WHERE properties->'url'->>0 = $1;|]
        enc = E.value E.text
        dec = D.maybeRow $ D.value D.jsonb
