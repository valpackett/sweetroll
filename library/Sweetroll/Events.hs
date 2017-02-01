{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

-- | All the things that happen on events such as "post updated", etc.
module Sweetroll.Events where

import           Sweetroll.Prelude
import           Sweetroll.Conf
import           Sweetroll.Monads
import           Sweetroll.Webmention.Send
import           Sweetroll.HTTPClient
import           Data.Maybe (fromJust)

type MonadSweetrollEvent μ = (MonadIO μ, MonadBaseControl IO μ, MonadCatch μ, MonadSweetroll μ)

onPostCreated ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostCreated category slug absUrl obj = do
  r ← onPostChanged category slug absUrl obj
  return r

onPostUpdated ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → Value → μ Value
onPostUpdated category slug absUrl oldObj obj = do
  r ← onPostChanged category slug absUrl obj
  return r

onPostChanged ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostChanged category _ absUrl obj = do
  notifyPuSHCategory category
  mentionResults ← sendWebmentions absUrl =<< entryWebmentions obj
  forM_ mentionResults $ \m → putStrLn $ "Sent mention result: " ++ tshow m
  let setSynd o (MentionSyndicated _ u) = o & key "properties" . key "syndication" . _Array %~ (cons (String u))
      setSynd o _ = o
      obj' = foldl' setSynd (obj & key "properties" %~ (ensureArrayProp "syndication")) mentionResults
  return $ obj' & key "properties" . key "syndication" . _Array %~ (fromList . nub . toList)

onPostDeleted ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Maybe Value → μ ()
onPostDeleted category slug absUrl mobj = do
  notifyPuSHCategory category
  case mobj of
    Just obj → do
      void $ sendWebmentions absUrl =<< entryWebmentions obj -- we send, they refetch and see 410
    _ → return ()

onPostUndeleted ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostUndeleted category slug absUrl obj = do
  r ← onPostChanged category slug absUrl obj
  return r


notifyPuSHCategory ∷ (MonadSweetrollEvent μ) ⇒ String → μ ()
notifyPuSHCategory catName = do
  notifyPuSH $ fromJust $ parseURI "/"
  notifyPuSH $ fromJust $ parseURI $ "/" ++ catName
  -- XXX

notifyPuSH ∷ (MonadSweetrollEvent μ) ⇒ URI → μ ()
notifyPuSH l = do
  hub ← getConfOpt pushHub
  case parseURI hub of
    Nothing → return ()
    Just hubURI → do
      base ← getBaseURI
      let pingURI = l `relativeTo` base
      resp ← runHTTP $ reqU hubURI >>= anyStatus
                       >>= postForm [ ("hub.mode", "publish"), ("hub.url", tshow pingURI) ]
                       >>= performWithVoid
      let status = case resp of
                     Right _ → "successfully"
                     Left e → "unsuccessfully (" ++ e ++ ")"
      putStrLn $ "PubSubHubbub notified " ++ status ++ " for " ++ tshow pingURI ++ " to hub " ++ tshow hubURI
      return ()
