{-# LANGUAGE TemplateHaskell #-}

-- | Types of records used in Sweetroll, mostly microformats2/micropub based.
-- URLs are used as references between records everywhere.
module Sweetroll.Types (module Sweetroll.Types) where

import           Data.Text.Lazy (Text)
import           Data.Time.Clock
import           Data.Aeson.TH

type Link = Text

data Card = Card
  { cardName      :: Text
  , cardUrl       :: Text
  , cardEmail     :: Maybe Text }
  deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Card)

data Cite = Cite
  { citeName      :: Text
  , citePublished :: UTCTime
  , citeAccessed  :: UTCTime
  , citeAuthor    :: Link
  , citeUrl       :: Text
  , citeContent   :: Text }
  deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Cite)

data Entry = Entry
  { entryName      :: Maybe Text
  , entrySummary   :: Maybe Text
  , entryContent   :: Maybe Text
  , entryPublished :: UTCTime
  , entryUpdated   :: UTCTime
  , entryTags      :: [Text]
  , entryAuthor    :: Maybe Link
  , entryInReplyTo :: Maybe Link
  , entryLikeOf    :: Maybe Link
  , entryRepostOf  :: Maybe Link }
  deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''Entry)
