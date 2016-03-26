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
onPostCreated category slug absUrl obj = do
  r ← onPostChanged category slug absUrl obj
  notifyPlugins "create" $ object [ "category" .= category, "slug" .= slug, "url" .= tshow absUrl, "obj" .= obj ]
  return r

onPostUpdated ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → Value → μ Value
onPostUpdated category slug absUrl oldObj obj = do
  r ← onPostChanged category slug absUrl obj
  notifyPlugins "update" $ object [ "category" .= category, "slug" .= slug, "url" .= tshow absUrl, "oldObj" .= oldObj, "obj" .= obj ]
  return r

onPostChanged ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Value → μ Value
onPostChanged category _ absUrl obj = do
  notifyPuSHCategory category
  mentionResults ← sendWebmentions absUrl =<< entryWebmentions obj
  let setSynd o (MentionSyndicated u) = o & key "properties" . key "syndication" . _Array %~ (cons (String u))
      setSynd o _ = o
      obj' = foldl' setSynd (obj & key "properties" %~ (ensureArrayProp "syndication")) mentionResults
  return $ obj' & key "properties" . key "syndication" . _Array %~ (fromList . nub . toList)

onPostDeleted ∷ (MonadSweetrollEvent μ) ⇒ String → String → URI → Maybe Value → μ ()
onPostDeleted category slug absUrl mobj = do
  notifyPuSHCategory category
  case mobj of
    Just obj → do
      void $ sendWebmentions absUrl =<< entryWebmentions obj -- we send, they refetch and see 410
      notifyPlugins "delete" $ object [ "category" .= category, "slug" .= slug, "url" .= tshow absUrl, "obj" .= mobj ]
    _ → return ()


notifyPuSHCategory ∷ (MonadSweetrollEvent μ) ⇒ String → μ ()
notifyPuSHCategory catName = do
  notifyPuSH $ permalink (Proxy ∷ Proxy IndexRoute)
  notifyPuSH $ permalink (Proxy ∷ Proxy CatRoute) catName Nothing Nothing
  notifyPuSH $ atomizeUri $ permalink (Proxy ∷ Proxy CatRoute) catName Nothing Nothing
  -- XXX: should send to unnamed combinations too
  catsToNotify ← liftM (filter (cs catName `isInfixOf`) . keys) $ getConfOpt categoryTitles
  forM_ catsToNotify $ \c → do
    notifyPuSH $ permalink (Proxy ∷ Proxy CatRoute) (cs c) Nothing Nothing
    notifyPuSH $ atomizeUri $ permalink (Proxy ∷ Proxy CatRoute) (cs c) Nothing Nothing

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
