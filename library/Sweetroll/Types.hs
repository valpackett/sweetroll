{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

-- | Types of records used in Sweetroll, mostly microformats2/micropub based.
-- URLs are used as references between records everywhere.
module Sweetroll.Types (module Sweetroll.Types) where

import           ClassyPrelude
import           Data.Aeson.TH
import           Data.Data

type Link = LText

data Card = Card
  { cardName      :: LText
  , cardUrl       :: LText
  , cardEmail     :: Maybe LText }
  deriving (Eq, Show, Data, Typeable)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Card)

data Cite = Cite
  { citeName      :: LText
  , citePublished :: UTCTime
  , citeAccessed  :: UTCTime
  , citeAuthor    :: Link
  , citeUrl       :: LText
  , citeContent   :: LText }
  deriving (Eq, Show, Data, Typeable)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Cite)

data Entry = Entry
  { entryName      :: Maybe LText
  , entrySummary   :: Maybe LText
  , entryContent   :: Maybe LText
  , entryPublished :: UTCTime
  , entryUpdated   :: UTCTime
  , entryTags      :: [LText]
  , entryAuthor    :: Maybe Link
  , entryInReplyTo :: Maybe Link
  , entryLikeOf    :: Maybe Link
  , entryRepostOf  :: Maybe Link }
  deriving (Eq, Show, Data, Typeable)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''Entry)
