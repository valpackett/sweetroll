{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE CPP, QuasiQuotes #-}

module Sweetroll.Conf (module Sweetroll.Conf) where

import           ClassyPrelude
import           Text.RawString.QQ
import           Web.Simple.Templates.Language
import           Sweetroll.Util (dropNonHtml)

data SweetrollConf = SweetrollConf
  {           layoutTemplate :: Template
  ,            entryTemplate :: Template
  ,         categoryTemplate :: Template
  ,                 siteName :: Text }

processTpl :: String -> Template
processTpl x = case compileTemplate $ dropNonHtml $ pack x of
  Left e -> Template { renderTemplate = \_ _ -> "Template compilation error: " ++ pack e }
  Right t -> t

-- cpp screws up line numbering, so we put this at the end
defaultSweetrollConf :: SweetrollConf
defaultSweetrollConf =  SweetrollConf {
    siteName = "A new Sweetroll site"
 ,  layoutTemplate = processTpl [r|
#include "../../templates/layout.html"
|], entryTemplate = processTpl [r|
#include "../../templates/entry.html"
|], categoryTemplate = processTpl [r|
#include "../../templates/category.html"
|]}
