{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs, FlexibleInstances #-}

module Sweetroll.Micropub.Request where

import           Sweetroll.Prelude hiding (first)
import           Control.Arrow (first)
import           Data.Attoparsec.Text as AP
import           Data.Aeson.Types (parseMaybe)
import           Servant

type ObjType = Text
type ObjProperties = Object
type ObjSyndication = Text
type ObjUrl = Text

data MicropubUpdate = ReplaceProps ObjProperties
                    | AddToProps ObjProperties
                    | DelFromProps ObjProperties
                    | DelProps [Text]
                    deriving (Eq, Show)

instance FromJSON [MicropubUpdate] where
  parseJSON v@(Object _) =
    let rplc = ReplaceProps <$> v ^? key "replace" . _Object
        addp = AddToProps <$> v ^? key "add" . _Object
        delf = DelFromProps <$> v ^? key "delete" . _Object
        delp = DelProps <$> mapMaybe (^? _String) . toList <$> v ^? key "delete" . _Array
     in return $ catMaybes [ rplc, addp, delf, delp ]
  parseJSON _ = mzero

data MicropubRequest = Create ObjType ObjProperties [ObjSyndication]
                     | Update ObjUrl [MicropubUpdate]
                     | Delete ObjUrl
                     | Undelete ObjUrl
                     deriving (Eq, Show)

instance FromJSON MicropubRequest where
  parseJSON v@(Object _) =
    case v ^? key "mp-action" . _String of
      Just "delete" →
        case v ^? key "url" . _String of
          Nothing → fail "Delete with no url"
          Just url → return $ Delete url
      Just "undelete" →
        case v ^? key "url" . _String of
          Nothing → fail "Unelete with no url"
          Just url → return $ Undelete url
      Just "update" →
        case v ^? key "url" . _String of
          Nothing → fail "Update with no url"
          Just url → return $ Update url $ fromMaybe [] $ parseMaybe parseJSON v
      Nothing → return $
        Create (fromMaybe "h-entry" $ firstStr v $ key "type")
               (fromMaybe (object [] ^. _Object) $ v ^? key "properties" . _Object)
               (v ^.. key "mp-syndicate-to" . values . _String)
      _ → fail "Unknown action type"
  parseJSON _ = mzero

instance FromFormUrlEncoded MicropubRequest where
  fromFormUrlEncoded f =
    case lookup "mp-action" f of
      Just "delete" →
        case lookup "url" f of
          Nothing → fail "Delete with no url"
          Just url → return $ Delete url
      Just "undelete" →
        case lookup "url" f of
          Nothing → fail "Undelete with no url"
          Just url → return $ Undelete url
      Just "update" →
        case lookup "url" f of
          Nothing → fail "Update with no url"
          Just url → return $ Update url $ fromMaybe [] $ parseMaybe parseJSON $ formToObject f
      Nothing →
        let v@(Object o') = formToObject f
            o = deleteMap "access_token" $ deleteMap "syndicate-to" $ deleteMap "mp-syndicate-to" $ deleteMap "h" o'
            h = "h-" ++ fromMaybe "entry" (lookup "h" f)
            synd = (v ^.. key "mp-syndicate-to" . values . _String) ++
                   (v ^.. key "syndicate-to" . values . _String)
         in Right $ Create h o synd
      _ → Left "Unknown action type"

formToObject ∷ [(Text, Text)] → Value
formToObject f = foldl' assignProp (object []) $ (map . first) parseKey f
  where parseKey x = fromMaybe [ x ] $ hush $ parseOnly formKey x
        assignProp (Object o) ([k], v) = Object $ insertWith concatJSON k (toJSON [ v ]) o
        assignProp (Object o) (k : k' : ks, v) = Object $ insertWith (\_ o' → assignProp o' (k' : ks, v)) k (assignProp (object []) (k' : ks, v)) o
        assignProp x _ = x
        concatJSON (Array v1) (Array v2) = Array $ v1 ++ v2
        concatJSON (Array v1) _ = Array v1
        concatJSON _ (Array v2) = Array v2
        concatJSON _ _ = Null

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
