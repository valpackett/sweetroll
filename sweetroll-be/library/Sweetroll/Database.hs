{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, TemplateHaskell, FlexibleContexts #-}
module Sweetroll.Database where

import           Sweetroll.Prelude hiding (Query)
import           Sweetroll.Conf (SweetrollConf)
import           Hasql.Query
import qualified Hasql.Pool as P
import qualified Hasql.Session as S
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import           Text.RawString.QQ

type Db = P.Pool

mkDb ∷ SweetrollConf → IO Db
mkDb _ = P.acquire (4, 5, "postgres://localhost/sweetroll?sslmode=disable")

useDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ S.Session ψ → μ (Either P.UsageError ψ)
useDb s = asks getter >>= \p → liftIO $ P.use p s

queryDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ χ → Query χ ψ → μ (Either P.UsageError ψ)
queryDb a b = useDb $ S.query a b

getObject ∷ Query Text (Maybe Value)
getObject = statement q enc dec True
  where q = [r|SELECT jsonb_build_object('type', type, 'properties', properties, 'children', children)
               FROM objects
               WHERE properties->'url'->>0 = $1;|]
        enc = E.value E.text
        dec = D.maybeRow $ D.value D.jsonb
