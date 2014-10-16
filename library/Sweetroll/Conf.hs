-- here be dragons
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- because Data.Setters
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell #-}

module Sweetroll.Conf (module Sweetroll.Conf) where

import           ClassyPrelude
import           Text.RawString.QQ
import           Data.Setters
import           Web.Simple.Templates.Language
import           Sweetroll.Util (dropNonHtml)

data SweetrollConf = SweetrollConf
  {           layoutTemplate :: Template
  ,            entryTemplate :: Template
  ,         categoryTemplate :: Template
  ,                 siteName :: Text
  ,           titleSeparator :: Text }

$(declareSetters ''SweetrollConf)

processTpl :: String -> Template
processTpl x = case compileTemplate $ dropNonHtml $ pack x of
  Left e -> Template { renderTemplate = \_ _ -> "Template compilation error: " ++ pack e }
  Right t -> t

loadTemplates :: SweetrollConf -> IO SweetrollConf
loadTemplates conf = foldM loadTpl conf tplsWithSetters
    where loadTpl c sf = readTpl c sf `catch` readFailHandler c
          readTpl c (setter, file) = do
            contents <- readFile $ "templates" </> file
            return $ setter (processTpl contents) c
          tplsWithSetters = [(setCategoryTemplate,   "category.html"),
                             (setEntryTemplate,      "entry.html"),
                             (setLayoutTemplate,     "layout.html")]

readFailHandler :: SweetrollConf -> IOError -> IO SweetrollConf
readFailHandler c _ = return c

-- cpp screws up line numbering, so we put this at the end
defaultSweetrollConf :: SweetrollConf
defaultSweetrollConf =  SweetrollConf {
    siteName = "A new Sweetroll site"
  , titleSeparator = " / "
  , layoutTemplate = processTpl [r|
#include "../../templates/layout.html"
|], entryTemplate = processTpl [r|
#include "../../templates/entry.html"
|], categoryTemplate = processTpl [r|
#include "../../templates/category.html"
|]}
