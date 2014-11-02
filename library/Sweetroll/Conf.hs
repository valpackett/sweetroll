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
  ,            indexTemplate :: Template
  ,           authorTemplate :: Template
  ,                 siteName :: Text
  ,                secretKey :: Text
  ,               httpsWorks :: Bool
  ,               domainName :: Text
  ,        indieAuthEndpoint :: String
  ,                 testMode :: Bool
  ,           titleSeparator :: Text }

$(declareSetters ''SweetrollConf)

s :: SweetrollConf -> Text
s conf = asText $ if httpsWorks conf then "s" else ""

baseURL :: SweetrollConf -> Text
baseURL conf = mconcat ["http", s conf, "://", domainName conf]

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
          tplsWithSetters = [ (setCategoryTemplate,   "category.html")
                            , (setEntryTemplate,      "entry.html")
                            , (setLayoutTemplate,     "layout.html")
                            , (setIndexTemplate,      "index.html")
                            , (setAuthorTemplate,     "author.html") ]

readFailHandler :: SweetrollConf -> IOError -> IO SweetrollConf
readFailHandler c _ = return c

-- cpp screws up line numbering, so we put this at the end
-- | The default SweetrollConf.
-- Actual defaults are in the executable!
defaultSweetrollConf :: SweetrollConf
defaultSweetrollConf =  SweetrollConf {
    siteName = ""
  , secretKey = "SECRET" -- the executable sets to a secure random value by default
  , httpsWorks = False
  , domainName = ""
  , indieAuthEndpoint = "http://127.0.0.1"
  , testMode = False
  , titleSeparator = " / "
  , layoutTemplate = processTpl [r|
#include "../../templates/layout.html"
|], entryTemplate = processTpl [r|
#include "../../templates/entry.html"
|], categoryTemplate = processTpl [r|
#include "../../templates/category.html"
|], indexTemplate = processTpl [r|
#include "../../templates/index.html"
|], authorTemplate = processTpl [r|
#include "../../templates/author.html"
|]}
