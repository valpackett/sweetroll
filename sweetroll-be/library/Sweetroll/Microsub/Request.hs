{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Sweetroll.Microsub.Request where

import           Sweetroll.Prelude
import           Web.FormUrlEncoded

type ChanName = Text
type ChanId = Text
type ObjId = Text
type FeedUrl = Text

data MicrosubRequest = MarkRead ChanId ObjId
                     | Remove ChanId ObjId
                     | Search Text
                     | Preview FeedUrl
                     | Follow ChanId FeedUrl
                     | Unfollow ChanId FeedUrl
                     | Mute ChanId FeedUrl
                     | Unmute ChanId FeedUrl
                     | Block ChanId FeedUrl
                     | CreateChannel ChanName
                     | UpdateChannel ChanId ChanName
                     | DeleteChannel ChanId
                     | ReorderChannels [ChanId]
                     deriving (Eq, Show)

instance FromForm MicrosubRequest where
  fromForm f' = let f = formList f' in
    case (lookup "action" f, lookup "method" f) of
      (Just "timeline", Just "mark_read") →
        case lookup "channel" f of
           Nothing → fail "Mark read with no channel"
           Just chan →
             -- TODO multi entry
             case lookup "last_read_entry" f <|> lookup "entry" f <|> lookup "entry[]" f of
               Nothing → fail "Mark read with no last_read_entry"
               Just entry → return $ MarkRead chan entry
      (Just "timeline", Just "remove") →
        case lookup "channel" f of
           Nothing → fail "Remove with no channel"
           Just chan →
             case lookup "entry" f <|> lookup "entry[]" f of
               Nothing → fail "Remove with no entry"
               Just entry → return $ Remove chan entry
      (Just "search", _) →
        case lookup "query" f of
           Nothing → fail "Search with no query"
           Just qry → return $ Search qry
      (Just "preview", _) →
        case lookup "url" f of
           Nothing → fail "Preview with no url"
           Just url → return $ Preview url
      (Just "follow", _) →
        case lookup "channel" f of
           Nothing → fail "Follow with no channel"
           Just chan →
             case lookup "url" f of
               Nothing → fail "Follow with no url"
               Just entry → return $ Follow chan entry
      (Just "unfollow", _) →
        case lookup "channel" f of
           Nothing → fail "Unfollow with no channel"
           Just chan →
             case lookup "url" f of
               Nothing → fail "Unfollow with no url"
               Just entry → return $ Unfollow chan entry
      (Just "mute", _) →
        case lookup "channel" f of
           Nothing → fail "Mute with no channel"
           Just chan →
             case lookup "url" f of
               Nothing → fail "Mute with no url"
               Just entry → return $ Mute chan entry
      (Just "unmute", _) →
        case lookup "channel" f of
           Nothing → fail "Unmute with no channel"
           Just chan →
             case lookup "url" f of
               Nothing → fail "Unmute with no url"
               Just entry → return $ Unmute chan entry
      (Just "block", _) →
        case lookup "channel" f of
           Nothing → fail "Block with no channel"
           Just chan →
             case lookup "url" f of
               Nothing → fail "Block with no url"
               Just entry → return $ Block chan entry
      (Just "channels", Nothing) →
        case lookup "name" f of
           Nothing → fail "Create/Update channel with no name"
           Just name →
              case lookup "channel" f of
                 Nothing → return $ CreateChannel name
                 Just chan → return $ UpdateChannel chan name
      (Just "channels", Just "delete") →
        case lookup "channel" f of
           Nothing → fail "Delete channel with no channel"
           Just chan → return $ DeleteChannel chan
      (Just "channels", Just "order") →
        return $ ReorderChannels $ lookupAll "channel" f' ++ lookupAll "channel[]" f'
      (a, m) → fail $ "Unsupported request: action=" ++ show a ++ " method=" ++ show m
