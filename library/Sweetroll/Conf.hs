-- here be dragons
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- because Data.Setters
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell #-}

module Sweetroll.Conf (
  module Sweetroll.Conf
, def
) where

import           ClassyPrelude
import           Text.RawString.QQ
import           Text.Pandoc.Options
import           Text.Highlighting.Kate.Styles (tango)
import           Data.Setters
import           Data.Default
import           Data.Aeson.TH
import           Web.Simple.Templates.Language
import           Sweetroll.Util (dropIncludeCrap)

data SweetrollTemplates = SweetrollTemplates
  {           layoutTemplate ∷ Template
  ,            entryTemplate ∷ Template
  ,         categoryTemplate ∷ Template
  ,            indexTemplate ∷ Template
  ,      entryInListTemplate ∷ Template
  ,           authorTemplate ∷ Template
  ,         notFoundTemplate ∷ Template
  ,             defaultStyle ∷ LByteString }

$(declareSetters ''SweetrollTemplates)

data SweetrollSecrets = SweetrollSecrets
  {                secretKey ∷ Text
  ,              adnApiToken ∷ String
  ,            twitterAppKey ∷ ByteString
  ,         twitterAppSecret ∷ ByteString
  ,       twitterAccessToken ∷ ByteString
  ,      twitterAccessSecret ∷ ByteString }

data SweetrollConf = SweetrollConf
  {                 siteName ∷ Text
  ,               domainName ∷ Text
  ,               httpsWorks ∷ Bool
  ,             itemsPerPage ∷ Int
  ,           titleSeparator ∷ Text
  ,   indieAuthRedirEndpoint ∷ String
  ,   indieAuthCheckEndpoint ∷ String -- Separated for debugging
  ,                  pushHub ∷ String
  ,                pushDelay ∷ Int
  ,               adnApiHost ∷ String
  ,           twitterApiHost ∷ String
  ,                 testMode ∷ Bool }

$(declareSetters ''SweetrollConf)
$(deriveJSON defaultOptions ''SweetrollConf)

s ∷ SweetrollConf → Text
s conf = if httpsWorks conf then "s" else ""

baseUrl ∷ SweetrollConf → Text
baseUrl conf = mconcat ["http", s conf, "://", domainName conf]

processTpl ∷ String → Template
processTpl x = case compileTemplate . dropIncludeCrap . pack $ x of
  Left e → Template { renderTemplate = \_ _ → "Template compilation error: " ++ pack e }
  Right t → t

loadTemplates ∷ IO SweetrollTemplates
loadTemplates = foldM loadTpl def tplsWithSetters
    where loadTpl c sf = readTpl c sf `catch` readFailHandler c
          readTpl c (setter, file) = do
            contents ← readFile $ "templates" </> file
            return $ setter (processTpl contents) c
          tplsWithSetters = [ (setCategoryTemplate,             "category.html")
                            , (setEntryTemplate,                "entry.html")
                            , (setLayoutTemplate,               "layout.html")
                            , (setIndexTemplate,                "index.html")
                            , (setEntryInListTemplate,          "entry-in-list.html")
                            , (setAuthorTemplate,               "author.html")
                            , (setNotFoundTemplate,             "404.html")
                            ]
          readFailHandler ∷ SweetrollTemplates → IOError → IO SweetrollTemplates
          readFailHandler c _ = return c

pandocReaderOptions ∷ ReaderOptions
pandocReaderOptions = def { readerExtensions = githubMarkdownExtensions
                          , readerSmart = True }

pandocWriterOptions ∷ WriterOptions
pandocWriterOptions = def { writerHtml5 = True
                          , writerEmailObfuscation = NoObfuscation
                          , writerHighlight = True
                          , writerHighlightStyle = tango
                          , writerIdentifierPrefix = "sr-" }

instance Default SweetrollConf where
  def = SweetrollConf {
        siteName                 = "A new Sweetroll website"
      , httpsWorks               = False
      , domainName               = ""
      , itemsPerPage             = 20
      , indieAuthCheckEndpoint   = "https://indieauth.com/auth"
      , indieAuthRedirEndpoint   = "https://indieauth.com/auth"
      , pushHub                  = "https://pubsubhubbub.superfeedr.com"
      , pushDelay                = 1
      , adnApiHost               = "https://api.app.net"
      , twitterApiHost           = "https://api.twitter.com/1.1"
      , testMode                 = False
      , titleSeparator           = " / " }

instance Default SweetrollSecrets where
  def = SweetrollSecrets {
        secretKey                = "SECRET" -- the executable sets to a secure random value by default
      , adnApiToken              = ""
      , twitterAppKey            = ""
      , twitterAppSecret         = ""
      , twitterAccessToken       = ""
      , twitterAccessSecret      = "" }

-- cpp screws up line numbering, so we put this at the end
instance Default SweetrollTemplates where
  def = SweetrollTemplates {
        layoutTemplate           = processTpl [r|
#include "../../templates/layout.html"
    |], entryTemplate            = processTpl [r|
#include "../../templates/entry.html"
    |], categoryTemplate         = processTpl [r|
#include "../../templates/category.html"
    |], indexTemplate            = processTpl [r|
#include "../../templates/index.html"
    |], entryInListTemplate      = processTpl [r|
#include "../../templates/entry-in-list.html"
    |], authorTemplate           = processTpl [r|
#include "../../templates/author.html"
    |], notFoundTemplate         = processTpl [r|
#include "../../templates/404.html"
    |], defaultStyle             = dropIncludeCrap $ asLByteString [r|
#include "../../templates/default-style.css"
|]}
