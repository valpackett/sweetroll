{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, FlexibleContexts #-}

module Sweetroll.Database where

import           Sweetroll.Prelude hiding (Query)
import           Sweetroll.Conf (SweetrollConf)
import           Servant (ServantErr, err500)
import           Hasql.Query
import qualified Hasql.Pool as P
import qualified Hasql.Session as S
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import           Text.RawString.QQ

type DbError = P.UsageError
type Db = P.Pool

mkDb ∷ SweetrollConf → IO Db
mkDb _ = P.acquire (4, 5, "postgres://localhost/sweetroll?sslmode=disable")

useDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ S.Session ψ → μ (Either P.UsageError ψ)
useDb s = asks getter >>= \p → liftIO $ P.use p s

queryDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ χ → Query χ ψ → μ (Either P.UsageError ψ)
queryDb a b = useDb $ S.query a b

guardDbError ∷ MonadError ServantErr μ ⇒ Either DbError α → μ α
guardDbError (Right x) = return x
guardDbError (Left x) = throwErrText err500 $ "Database error: " ++ cs (show x)

getObject ∷ Query Text (Maybe Value)
getObject = statement q enc dec True
  where q = [r|SELECT objects_smart_fetch($1, null, 0, null, null, null)|]
        enc = E.value E.text
        dec = D.maybeRow $ D.value D.jsonb

upsertObject ∷ Query Value ()
upsertObject = statement q enc dec True
  where q = [r|SELECT objects_normalized_upsert($1)|]
        enc = E.value E.jsonb
        dec = D.unit

deleteObject ∷ Query Text ()
deleteObject = statement q enc dec True
  where q = [r|UPDATE objects SET deleted = True WHERE properties->'url'->>0 = $1|]
        enc = E.value E.text
        dec = D.unit

undeleteObject ∷ Query Text ()
undeleteObject = statement q enc dec True
  where q = [r|UPDATE objects SET deleted = False WHERE properties->'url'->>0 = $1|]
        enc = E.value E.text
        dec = D.unit
