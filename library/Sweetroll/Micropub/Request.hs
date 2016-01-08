{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs #-}

module Sweetroll.Micropub.Request (
  ObjType
, ObjProperties
, ObjSyndication
, MicropubRequest (..)
, formToObject
) where

import           ClassyPrelude hiding (first)
import           Control.Arrow (first)
import           Control.Error.Util (hush)
import           Data.Aeson
import           Data.Attoparsec.Text as AP
import           Servant

type ObjType = Text
type ObjProperties = Object
type ObjSyndication = Text

data MicropubRequest = Create ObjType ObjProperties [ObjSyndication]

instance FromJSON MicropubRequest where
  parseJSON _ = mzero

formKey ∷ Parser [Text]
formKey = do
  firstKey ← AP.takeWhile (/= '[')
  restKeys ← many' $ do
    void $ char '['
    s ← AP.takeWhile (/= ']')
    void $ char ']'
    return s
  void $ option '_' $ char '[' >> char ']'
  return $ firstKey : filter (not . null) restKeys

formToObject ∷ [(Text, Text)] → Value
formToObject f = foldl' assign (object []) $ (map . first) parseKey f
  where parseKey x = fromMaybe [ x ] $ hush $ parseOnly formKey x
        assign (Object o) ([k], v) = Object $ insertWith concatJSON k (toJSON [ v ]) o
        assign (Object o) (k : k' : ks, v) = Object $ insertWith (\_ o' → assign o' (k' : ks, v)) k (assign (object []) (k' : ks, v)) o
        assign x _ = x
        concatJSON (Array v1) (Array v2) = Array $ v1 ++ v2
        concatJSON (Array v1) _ = Array v1
        concatJSON _ (Array v2) = Array v2
        concatJSON _ _ = Null

instance FromFormUrlEncoded MicropubRequest where
  fromFormUrlEncoded f =
    case lookup "mp-action" f of
      Nothing → let (Object o') = formToObject f
                    o = deleteMap "h" o'
                    h = "h-" ++ fromMaybe "entry" (lookup "h" f) in
                      Right $ Create h o []
      _ → Left "Unknown action type"
