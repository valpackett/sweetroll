{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving, UnicodeSyntax #-}

module Sweetroll.Style (
  allCss
, sweetrollStyle
) where

import           ClassyPrelude hiding (rem, (**), (<>))
import           Text.Highlighting.Kate.Format.HTML (styleToCss)
import           Clay hiding (round)
import           Clay.Stylesheet
import qualified Clay.Media as M
import qualified Clay.Text as T
import           Data.Stringable
import           Text.Pandoc
import           Sweetroll.Conf

allCss, pandocCss, sweetrollCss ∷ LByteString

allCss = pandocCss ++ sweetrollCss
pandocCss = toLazyByteString . styleToCss . writerHighlightStyle $ pandocWriterOptions
sweetrollCss = toLazyByteString . renderWith compact [] $ sweetrollStyle

sweetrollStyle ∷ Css
sweetrollStyle = do
  star ? boxSizing borderBox

  html ? do
    "-ms-text-size-adjust" -: "100%"
    "-webkit-text-size-adjust" -: "100%"
    fontFamily ["Helvetica Neue"] [sansSerif]
    "line-height" -: "1.3"
    backgroundColor "#ffffff"
    color "#343434"

  body ? do
    sym margin nil
    "min-height" -: "100vh"
    display flex
    "flex-direction" -: "column"
    hyphens autoHyphens
    "word-wrap" -: "break-word"

  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
    marginTop nil
    "line-height" -: "1"
    textRendering optimizeLegibility

  ".giga"  ? fSize 90
  ".mega"  ? fSize 72
  ".alpha" ? fSize 60
  ".beta"  ? fSize 48
  h1 <> ".gamma"   ? fSize 36
  h2 <> ".delta"   ? fSize 24
  h3 <> ".epsilon" ? fSize 21
  h4 <> h5 <> h6 <> ".zeta" ? fSize 18

  dfn ? fontStyle italic
  b <> strong ? fontWeight bold
  input <> abbr <> blockquote <> code <> kbd <> q <> samp <> var ? hyphens noHyphens
  article <> aside <> details <> figcaption <> figure <> footer <> header <> hgroup <> main_ <> menu <> nav <> section <> summary ? display block
  audio <> canvas <> progress <> video ? do
    display inlineBlock
    verticalAlign baseline
  abbr ? do
    fontVariant smallCaps
    "font-weight" -: "600"
    color gray
    "@title" & do
      "border-bottom" -: "dotted 1px"
      hover & cursor help
  p <> pre <> figure <> blockquote ? do
    margin auto auto (em 1.5) auto
    lastChild & marginBottom (em 0.5)
  mark ? do
    backgroundColor "#ff0"
    color black
  small ? fontSize (pct 80)
  sub <> sup ? do
    fontSize $ pct 80
    lineHeight nil
    position relative
    verticalAlign baseline
  sub ? top (em (-0.5))
  sup ? bottom (em (-0.25))
  img ? do
    borderWidth nil
    maxWidth $ pct 100
  pre ? do
    overflow auto
    whiteSpace T.pre
  code <> kbd <> pre <> samp ? do
    fontSize $ em 1
    fontFamily ["Menlo", "Consolas"] [monospace]
  pre ** code ? do
    overflow auto
    whiteSpace T.pre
  table ? do
    borderCollapse collapse
  td <> th ? sym padding nil
  ul ? listStyleType square
  ul <> ol ? do
    paddingLeft nil
    listStylePosition inside
  ".fa-l" ? marginRight (em 0.25)
  ".fa-r" ? marginLeft (em 0.25)
  a ? do
    color "#0074df"
    transition "color" (sec 0.3) easeInOut (sec 0)
    textDecoration none
  (a # hover) <> (a # focus) <> (a # active) ? do
    color "#7fdbff"
    textDecoration underline
  a # ".self-link" ? do
    color "#343434"
    textDecoration none

  ".site-content" <> ".index-main" ? do
    "display" -: "-webkit-flex"
    display flex
    "-webkit-flex-direction" -: "column"
    "flex-direction" -: "column"

  ".site-content" ? flexValue 1
  ".site-author" ? orderValue 10

  ".site-footer" ? do
    sym2 padding (em 1) nil
    fontSize $ pct 90
    color "#777"

  ".site-header" ? ("padding" -: "2em 0")

  pageParts ? do
    width $ pct 96
    "margin" -: "0 auto"

  ".entry-in-list" ? do
    listStyleType none
    display block
    padding (em 1) 0 (em 0.5) 0
    borderBottom solid (px 1) transparent
    borderTop solid (px 1) $ rgb  57 204 204
    borderTop solid (px 1) $ rgba 57 204 204 134
    transition "border-color" (sec 0.3) easeInOut (sec 0)
    hover & borderColor "#2ecc40"
    firstChild & ("border-top" -: "none")
    ".entry-footer" ? do
      fontSize $ pct 90
      textAlign . alignSide $ sideRight
    ".entry-footer" <> ".entry-footer a" ? do
      fontSize $ pct 90
      color "#aaa"
  ".entry-in-list" # hover |+ ".entry-in-list" ? borderColor transparent

  ".note-entry" ? do
    border solid (px 1) $ rgb 57 204 204
    borderColor $ rgba 57 204 204 79
    ".entry-header" ? do
      fontSize $ pct 90
      marginBottom $ em 1
    ".entry-header" <> ".entry-header a" ? color "#aaa"
    ".entry-content" ? fontSize (pct 110)
    ".entry-footer" ? do
      borderTop solid (px 1) $ rgb 57 204 204
      borderColor $ rgba 57 204 204 82

  ".entry-footer" ? do
    color "#999"
    fontSize $ pct 95
    "line-height" -: "1.7"

  ".entry-main" ? do
    ".entry-header" <> ".entry-content" <> ".entry-footer" ? do
      sym margin $ px 16
      sym margin $ rem 1
    ".entry-footer" ? do
      "a" ? marginLeft (em 0.5)
      ".entry-actions" ** "indie-action" # firstChild ** a ? marginLeft nil

  ".entry-footer" ** h2 ? do
    fontSize $ pct 100
    display inline

  ".entry-syndication" ** (ul <> li) ? display inline

  ".social-profiles" ? do
    listStyleType none
    "line-height" -: "1.7"

  query M.screen [M.minWidth $ em 32] $ do
    html ? ("line-height" -: "1.4")
    pageParts ? width (pct 75)
    ".entry-in-list" ** ".entry-footer" ? paddingRight (em 1)

  query M.screen [M.minWidth $ em 56] $ do
    pageParts ? width (pct 90)
    body ? fontSize (pct 105)
    ".site-content" <> ".index-main" ? do
      "-webkit-flex-direction" -: "row"
      "flex-direction" -: "row"
    ".site-author" ? do
      flexValue 1
      orderValue 0
    ".site-main" ? do
      flexValue 3
      paddingLeft $ em 1
      overflowX auto
    ".index-category" ? do
      flexValue 1
      minWidth $ pct 30
      maxWidth $ pct 100
      padding nil (em 1) nil nil
      lastChild & ("padding" -: "0")
    "#author-link" ? display none

  query M.screen [M.minWidth $ em 80] $ do
    pageParts ? width (pct 90)
    ".site-header" ? sym2 padding (em 3) nil
    ".site-main" ? paddingLeft (em 3.4)
    ".note-entry" ? maxWidth (em 60)
    ".entry-in-list" ? do
      padding (em 2) nil (em 1) nil
      ".entry-footer" ? marginTop (em 1)

  query M.screen [M.minWidth $ em 80] $ do
    pageParts ? width (pct 65)

pageParts ∷ Selector
pageParts = ".site-header" <> ".site-content" <> ".site-footer"

fSize, flexValue, orderValue ∷ Integer → Css
fSize x = do
  fontSize . px  $ x
  fontSize . rem $ fromIntegral x / 18.0
  marginBottom . px . round $ 29.7 / fromIntegral x * (18.0 ∷ Double)
  marginBottom . rem $ 29.7 / fromIntegral x

flexValue x = do
  "-webkit-flex" -: pack (show x)
  "flex" -: pack (show x)

orderValue x = do
  "-webkit-order" -: pack (show x)
  "order" -: pack (show x)

newtype HyphensType = HyphensType Value
  deriving (Val, Inherit)

autoHyphens, noHyphens ∷ HyphensType
autoHyphens = HyphensType "auto"
noHyphens   = HyphensType "none"

hyphens ∷ HyphensType → Css
hyphens = prefixed (browsers ++ "hyphens")

infixl 9 <>
(<>) ∷ Monoid μ ⇒ μ → μ → μ
(<>) = (++)
