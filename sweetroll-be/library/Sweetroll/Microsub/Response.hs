{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DeriveGeneric #-}

module Sweetroll.Microsub.Response where

import           Sweetroll.Prelude

data Channel = Channel { uid ∷ Text
                       , name ∷ Text
                       , unread ∷ Int }
                       deriving (Generic)

instance ToJSON Channel where
  toEncoding = genericToEncoding defaultOptions

compactChannel ∷ Value → Maybe Channel
compactChannel x = do
  url ← firstStr x (key "properties" . key "url")
  name ← firstStr x (key "properties" . key "name")
  return $ Channel url name 0

data Subscription = Subscription { url ∷ Text }

instance ToJSON Subscription where
  toJSON (Subscription url) = toJSON $ object [ "type" .= asText "feed", "url" .= url ]

compactSubscription ∷ Value → Maybe Subscription
compactSubscription x = do
  url ← firstStr x (key "feed")
  return $ Subscription url

data Paging = Paging { after ∷ Maybe Text
                     , before ∷ Maybe Text }
                     deriving (Generic)

instance ToJSON Paging where
  toEncoding = genericToEncoding defaultOptions

data MicrosubResponse = Created Channel
                      | Channels [Channel]
                      | Subscriptions [Subscription]
                      | Subscribed Subscription
                      | Entries [Value] (Maybe Paging)

instance ToJSON MicrosubResponse where
  toJSON (Created chan) = toJSON chan
  toJSON (Channels chans) = toJSON $ object [ "channels" .= chans ]
  toJSON (Subscriptions subs) = toJSON $ object [ "items" .= subs ]
  toJSON (Subscribed sub) = toJSON sub
  toJSON (Entries ents (Just pg)) = toJSON $ object [ "items" .= ents, "paging" .= pg ]
  toJSON (Entries ents _) = toJSON $ object [ "items" .= ents ]
