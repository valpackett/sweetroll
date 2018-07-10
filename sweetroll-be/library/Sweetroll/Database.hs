{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, FlexibleContexts, GADTs #-}

module Sweetroll.Database where

import           Sweetroll.Prelude
import           Sweetroll.Conf
import           Hasql.Statement
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

queryDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ χ → Statement χ ψ → μ (Either P.UsageError ψ)
queryDb a b = useDb $ S.statement a b

transactDb ∷ (Has Db α, MonadReader α μ, MonadIO μ) ⇒ T.Transaction ψ → μ (Either P.UsageError ψ)
transactDb t = useDb $ TS.transaction TS.RepeatableRead TS.Write t

queryTx ∷ χ → Statement χ ψ → T.Transaction ψ
queryTx = T.statement

guardDbError ∷ MonadThrow μ ⇒ Either DbError α → μ α
guardDbError (Right x) = return x
guardDbError (Left x) = throwErrText err500 $ "Database error: " ++ cs (show x)

guardTxError ∷ MonadThrow μ ⇒ Either P.UsageError α → μ α
guardTxError (Right x) = return x
guardTxError (Left x) = throwErrText err500 $ "Database error: " ++ cs (show x)

getObject ∷ Statement Text (Maybe Value)
getObject = Statement q enc dec True
  where q = [r|SELECT mf2.objects_smart_fetch($1, null, 0, null, null, null)|]
        enc = E.param E.text
        dec = D.rowMaybe $ D.column D.jsonb

upsertObject ∷ Statement Value ()
upsertObject = Statement q enc dec True
  where q = [r|SELECT mf2.objects_normalized_upsert($1)|]
        enc = E.param E.jsonb
        dec = D.unit

deleteObject ∷ Statement Text ()
deleteObject = Statement q enc dec True
  where q = [r|UPDATE mf2.objects SET deleted = True WHERE properties->'url'->>0 = $1|]
        enc = E.param E.text
        dec = D.unit

undeleteObject ∷ Statement Text ()
undeleteObject = Statement q enc dec True
  where q = [r|UPDATE mf2.objects SET deleted = False WHERE properties->'url'->>0 = $1|]
        enc = E.param E.text
        dec = D.unit

-- Local domains are domains managed by this Sweetroll instance.
-- We don't want to fetch-parse-overwrite their entries!
getLocalDomains ∷ Statement () [Text]
getLocalDomains = Statement q enc dec True
  where q = [r|SELECT properties->'url'->>0 FROM mf2.objects WHERE properties->'site-settings' IS NOT NULL|]
        enc = E.unit
        dec = D.rowList $ D.column D.text

notifyWebmention ∷ Statement Value ()
notifyWebmention = Statement q enc dec True
  where q = [r|SELECT pg_notify('webmentions', $1::text)|]
        enc = E.param E.json
        dec = D.unit

getFeeds ∷ Statement Text (Maybe Value)
getFeeds = Statement q enc dec True
  where q = [r|SELECT x.obj FROM
               (SELECT set_config('mf2sql.current_user_url', $1, true),
                mf2.objects_fetch_feeds($1) obj) AS x|]
        enc = E.param E.text
        dec = D.rowMaybe $ D.column D.jsonb

data FeedQuery = FeedQuery { fUserDomain ∷ Text
                           , feedUrl ∷ Text
                           , fetchLimit ∷ Int32
                           , before ∷ Maybe Text
                           , after ∷ Maybe Text }

getFeed ∷ Statement FeedQuery (Maybe Value)
getFeed = Statement q enc dec True
  where q = [r|SELECT x.obj FROM
               (SELECT set_config('mf2sql.current_user_url', $5, true),
                mf2.objects_smart_fetch($1, null, $2::int, $3::timestamptz, $4::timestamptz, null) obj) AS x|]
        enc = contramap feedUrl (E.param E.text) ++
              contramap fetchLimit (E.param E.int4) ++
              contramap before (E.nullableParam E.text) ++
              contramap after (E.nullableParam E.text) ++
              contramap fUserDomain (E.param E.text)
        dec = D.rowMaybe $ D.column D.jsonb

getSubscriptionUrls ∷ Statement () [Maybe Text]
getSubscriptionUrls = Statement q enc dec True
  where q = [r|SELECT DISTINCT jsonb_array_elements(coalesce(properties->'subscriptions', '[]'::jsonb))->>'feed'
               FROM mf2.objects
               WHERE type @> '{h-x-reader-channel}'|]
        enc = E.unit
        dec = D.rowList $ D.nullableColumn D.text

data FeedEntryAdd = FeedEntryAdd { feedUrls ∷ [Text] -- with and without trailing slash
                                 , entryUrls ∷ [Text] }

addEntriesToFeeds ∷ Statement FeedEntryAdd ()
addEntriesToFeeds = Statement q enc dec True
  where q = [r|UPDATE mf2.objects
               SET properties = jsonb_set(properties, '{subscriptions}', (
                 SELECT jsonb_agg(
                   CASE WHEN subs->>'feed' = ANY($1)
                   THEN jsonb_set(subs, '{entries}', (
                     SELECT DISTINCT jsonb_agg(e)
                     FROM (SELECT jsonb_array_elements_text(subs->'entries') e
                          UNION SELECT unnest($2)) x))
                   ELSE subs END)
                 FROM jsonb_array_elements(coalesce(properties->'subscriptions', '[]'::jsonb)) subs))
               WHERE type @> '{h-x-reader-channel}'
               AND deleted = False
               AND properties->'subscriptions' @> ANY(
                 SELECT jsonb_build_array(jsonb_build_object('feed', x)) FROM (SELECT unnest($1) x) x)|]
        enc = contramap feedUrls (E.param $ E.array $ E.dimension foldl' $ E.element E.text) ++
              contramap entryUrls (E.param $ E.array $ E.dimension foldl' $ E.element E.text)
        dec = D.unit


data FeedSubAdd = FeedSubAdd { sAllowedAcl ∷ [Text]
                             , channelUrl ∷ Text
                             , subUrl ∷ [Text] }

addSubscriptionToFeed ∷ Statement FeedSubAdd ()
addSubscriptionToFeed = Statement q enc dec True
  where q = [r|UPDATE mf2.objects
               SET properties = jsonb_set(properties, '{subscriptions}',
                 coalesce(properties->'subscriptions', '[]'::jsonb) || jsonb_build_array(jsonb_build_object('feed', ($2)[1])))
               WHERE type @> '{h-x-reader-channel}'
               AND deleted = False
               AND (acl && $3)
               AND properties->'url'->>0 = trim(both from $1, '"')
               AND NOT properties->'subscriptions' @> ANY(
                 SELECT jsonb_build_array(jsonb_build_object('feed', x)) FROM (SELECT unnest($2) x) x)|]
        enc = contramap channelUrl (E.param E.text) ++
              contramap subUrl (E.param $ E.array $ E.dimension foldl' $ E.element E.text) ++
              contramap sAllowedAcl (E.param $ E.array $ E.dimension foldl' $ E.element E.text)
        dec = D.unit
