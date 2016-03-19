{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

-- | All the things that happen on events such as "post updated", etc.
module Sweetroll.Events where

import           Sweetroll.Prelude
import           Sweetroll.Conf
import           Sweetroll.Routes
import           Sweetroll.Monads
import           Sweetroll.Webmention.Send
import           Sweetroll.HTTPClient

type MonadSweetrollEvent μ = (MonadIO μ, MonadBaseControl IO μ, MonadThrow μ, MonadSweetroll μ)

onPostCreated ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostCreated = onPostUpdated

onPostUpdated ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostUpdated category _ absUrl obj = do
  notifyPuSHCategory category
  mentionResults ← sendWebmentions absUrl =<< entryWebmentions obj
  let setSynd o (MentionSyndicated u) = o & key "properties" . key "syndication" . _Array %~ (cons (String u))
      setSynd o _ = o
      obj' = foldl' setSynd (obj & key "properties" %~ (ensureArrayProp "syndication")) mentionResults
  return $ obj' & key "properties" . key "syndication" . _Array %~ (fromList . nub . toList)

onPostDeleted ∷ (MonadSweetrollEvent μ) ⇒ String → String → μ ()
onPostDeleted category _ =
  notifyPuSHCategory category


notifyPuSHCategory ∷ (MonadSweetrollEvent μ) ⇒ String → μ ()
notifyPuSHCategory catName = do
  notifyPuSH $ permalink (Proxy ∷ Proxy IndexRoute)
  notifyPuSH $ permalink (Proxy ∷ Proxy CatRouteE) catName
  notifyPuSH $ atomizeUri $ permalink (Proxy ∷ Proxy CatRouteE) catName
  -- XXX: should send to unnamed combinations too
  catsToNotify ← liftM (filter (cs catName `isInfixOf`) . keys) $ getConfOpt categoryTitles
  forM_ catsToNotify $ \c → do
    notifyPuSH $ permalink (Proxy ∷ Proxy CatRouteE) $ cs c
    notifyPuSH $ atomizeUri $ permalink (Proxy ∷ Proxy CatRouteE) $ cs c

notifyPuSH ∷ (MonadSweetrollEvent μ) ⇒ URI → μ ()
notifyPuSH l = do
  hub ← getConfOpt pushHub
  case parseURI hub of
    Nothing → return ()
    Just hubURI → do
      base ← getConfOpt baseURI
      let pingURI = l `relativeTo` base
      resp ← runHTTP $ reqU hubURI >>= anyStatus
                       >>= postForm [ ("hub.mode", "publish"), ("hub.url", tshow pingURI) ]
                       >>= performWithVoid
      let status = case resp of
                     Right _ → "successfully"
                     Left e → "unsuccessfully (" ++ e ++ ")"
      putStrLn $ "PubSubHubbub notified " ++ status ++ " for " ++ tshow pingURI ++ " to hub " ++ tshow hubURI
      return ()
