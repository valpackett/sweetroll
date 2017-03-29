{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, GADTs, FlexibleInstances #-}

module Sweetroll.Micropub.Request where

import           Sweetroll.Prelude
import           Data.Aeson.Types (parseMaybe)
import           Web.FormUrlEncoded hiding (parseMaybe)

type ObjType = Text
type ObjProperties = Object
type ObjSyndication = Text
type ObjUrl = Text

data MicropubUpdate = ReplaceProps ObjProperties
                    | AddToProps ObjProperties
                    | DelFromProps ObjProperties
                    | DelProps [Text]
                    deriving (Eq, Show)

instance {-# OVERLAPPING #-} FromJSON [MicropubUpdate] where
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
    -- TODO: remove mp-action, was removed from spec
    case v ^? key "action" . _String <|> v ^? key "mp-action" . _String of
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

-- TODO: remove form-encoded update requests, was removed from spec
instance FromForm MicropubRequest where
  fromForm f' = let f = formList f' in
    case lookup "action" f <|> lookup "mp-action" f of
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
            synd = nub $ (v ^.. key "mp-syndicate-to" . values . _String) ++
                         (v ^.. key "syndicate-to" . values . _String)
                         -- no syndicate-to[], the [] is handled in formToObject
         in Right $ Create h o synd
      _ → Left "Unknown action type"
