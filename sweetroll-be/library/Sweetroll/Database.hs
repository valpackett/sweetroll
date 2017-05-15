{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, FlexibleContexts #-}

module Sweetroll.Database where

import           Sweetroll.Prelude hiding (Query)
import           Sweetroll.Conf
import           Hasql.Query
import qualified Hasql.Pool as P
import qualified Hasql.Session as S
import qualified Hasql.Transaction.Sessions as TS
import qualified Hasql.Transaction as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

type DbError = P.UsageError
type Db = P.Pool

mkDb ∷ SweetrollConf → IO Db
mkDb conf = P.acquire (4, 5, databaseUrl conf)

useDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ S.Session ψ → μ (Either P.UsageError ψ)
useDb s = asks getter >>= \p → liftIO $ P.use p s

queryDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ χ → Query χ ψ → μ (Either P.UsageError ψ)
queryDb a b = useDb $ S.query a b

transactDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ T.Transaction ψ → μ (Either P.UsageError ψ)
transactDb t = useDb $ TS.transaction T.RepeatableRead T.Write t

queryTx ∷ χ → Query χ ψ → T.Transaction ψ
queryTx = T.query

guardDbError ∷ MonadError ServantErr μ ⇒ Either DbError α → μ α
guardDbError (Right x) = return x
guardDbError (Left x) = throwErrText err500 $ "Database error: " ++ cs (show x)

guardTxError ∷ MonadError ServantErr μ ⇒ Either P.UsageError α → μ α
guardTxError (Right x) = return x
guardTxError (Left x) = throwErrText err500 $ "Database error: " ++ cs (show x)

getObject ∷ Query Text (Maybe Value)
getObject = statement q enc dec True
  where q = [r|SELECT mf2.objects_smart_fetch($1, null, 0, null, null, null)|]
        enc = E.value E.text
        dec = D.maybeRow $ D.value D.jsonb

upsertObject ∷ Query Value ()
upsertObject = statement q enc dec True
  where q = [r|SELECT mf2.objects_normalized_upsert($1)|]
        enc = E.value E.jsonb
        dec = D.unit

deleteObject ∷ Query Text ()
deleteObject = statement q enc dec True
  where q = [r|UPDATE mf2.objects SET deleted = True WHERE properties->'url'->>0 = $1|]
        enc = E.value E.text
        dec = D.unit

undeleteObject ∷ Query Text ()
undeleteObject = statement q enc dec True
  where q = [r|UPDATE mf2.objects SET deleted = False WHERE properties->'url'->>0 = $1|]
        enc = E.value E.text
        dec = D.unit

-- Local domains are domains managed by this Sweetroll instance.
-- We don't want to fetch-parse-overwrite their entries!
getLocalDomains ∷ Query () [Text]
getLocalDomains = statement q enc dec True
  where q = [r|SELECT properties->'url'->>0 FROM mf2.objects WHERE properties->'site-settings' IS NOT NULL|]
        enc = E.unit
        dec = D.rowsList $ D.value D.text

notifyWebmention ∷ Query Value ()
notifyWebmention = statement q enc dec True
  where q = [r|SELECT pg_notify('webmentions', $1::text)|]
        enc = E.value E.json
        dec = D.unit
