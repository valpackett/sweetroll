{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving, UnicodeSyntax #-}

module Sweetroll.Style (
  allCss
, sweetrollStyle
) where

import           ClassyPrelude hiding (span, rem, (**), (<>))
import           Text.Highlighting.Kate.Format.HTML (styleToCss)
import           Text.Pandoc
import           Data.String.Conversions (cs)
import           Clay hiding (round)
import           Clay.Stylesheet
import qualified Clay.Media as M
import qualified Clay.Text as T
import           Sweetroll.Conf

allCss, pandocCss, sweetrollCss ∷ LByteString

allCss = pandocCss ++ sweetrollCss
pandocCss = cs . styleToCss . writerHighlightStyle $ pandocWriterOptions
sweetrollCss = cs . renderWith compact [] $ sweetrollStyle

sweetrollStyle ∷ Css
sweetrollStyle = do
  star ? boxSizing borderBox

  html ? do
    "-ms-text-size-adjust" -: "100%"
    "-webkit-text-size-adjust" -: "100%"
    fontFamily ["Helvetica Neue"] [sansSerif]
    fontSize $ px 16
    "line-height" -: "1.55"
    backgroundColor "#ffffff"
    color "#3b3b3b"

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
    fontSize $ pct 75
    lineHeight nil
    position relative
    verticalAlign baseline
  sup ? top (em (-0.5))
  sub ? bottom (em (-0.25))
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
  "[fragmention]" ? do
    backgroundColor $ rgba 246 242 195 153
  "fragmention-exact" ? do
    backgroundColor "#FFF8A7"
  "blockquote" ? do
    paddingLeft $ em 0.4
    borderLeft solid (em 0.2) $ rgb 67 214 214

  ".site-content" <> ".index-main" ? do
    "display" -: "-webkit-flex"
    display flex
    "-webkit-flex-direction" -: "column"
    "flex-direction" -: "column"

  ".site-content" ? flexValue 1

  ".site-footer" ? do
    sym2 padding (em 1) nil
    fontSize $ pct 90
    color "#777"

  ".site-header" ? sym2 padding (em 2) nil

  pageParts ? do
    width $ pct 96
    "margin" -: "0 auto"

  ".site-main" ? marginBottom (em 2)

  ".index-category" ? marginBottom (em 2)

  ".entry-in-list" <> ".note-entry" ? do
    boxShadow 0 (px 3) (px 13) $ rgba 0 0 0 64
    sym borderRadius $ px 3

  ".note-in-list" ** ".permalink" ? do
    display block

  ".entry-in-list" ? do
    listStyleType none
    display block
    sym padding $ em 0.6
    sym2 margin (em 1.4) nil
    ".entry-footer" ? do
      textAlign . alignSide $ sideRight
    ".entry-footer" <> ".entry-footer a" ? grayedOut

  ".article-in-list" ? do
    ".p-name" ? fontSize (em 1.35)

  ".note-main" ? do
    paddingTop $ em 1

  ".entry-header" <> ".entry-header a" ? grayedOut

  ".note-entry" ? do
    paddingTop $ em 0.2
    paddingBottom $ em 0.2
    entryParts ? sym margin (em 1)
    ".entry-header" ? do
      fontSize $ pct 90
      paddingTop $ em 0.4
    ".entry-content" ? fontSize (pct 110)
    ".entry-footer" ? do
      marginTop $ em 1.4
      paddingTop $ em 1.28
    ".entry-footer" <> ".entry-responses" ? do
      borderTop solid (px 1) $ rgb 57 204 204
      borderColor $ rgba 57 204 204 82

  ".entry-response" ? do
    sym2 padding (em 0.8) (em 0.4)
    nthChild "even" & do
      backgroundColor $ rgba 230 230 230 60
  ".entry-response-header" ? do
    "img" ? do
      maxHeight $ em 2
      maxWidth $ em 2
      verticalAlign middle
      marginBottom $ em 0.15
      sym borderRadius $ px 3
  ".entry-response-content" ? do
    marginLeft $ em 0.899
    marginTop $ em 0.4
  ".entry-response-responses" ? do
    sym margin $ em 0.6
    marginLeft $ em 1.4

  ".article-entry" ? do
    ".entry-header a" ? color "#222"
    entryParts ? sym2 margin (em 1) nil

  ".entry-footer" ? do
    paddingTop $ em 0.5
    color "#999"
    "line-height" -: "2.2"
    "a" ? do
      sym2 padding (em 1) (em 0.6)
    "indie-action" # firstChild ** a ? marginLeft nil

  ".entry-main" ? do
    ".entry-header" ? marginTop (em 0)

  ".entry-footer" ** h2 ? do
    fontSize $ pct 100
    display inline

  ".entry-syndication" ** (ul <> li) ? display inline

  ".reference-context" ? do
    sym padding $ em 0.4
    sym margin $ em 0.4
    backgroundColor "#efefef"
    color "#606060"
    "a" ? color "#0064cf"
    "blockquote" ? do
      marginTop $ em 0.4

  ".social-profiles" ? do
    listStyleType none
    "line-height" -: "1.7"

  "#author-link" ? do
    display block
    fontSize $ rem 2
    marginTop $ rem 1
    textDecoration none
    "span" ? color transparent
    focus & "span" ? color inherit

  query M.screen [M.minWidth $ em 32] $ do
    html ? ("line-height" -: "1.4")
    pageParts ? width (pct 75)
    ".entry-in-list" ** ".entry-footer" ? paddingRight (em 1)

  query M.screen [M.minWidth $ em 56] $ do
    -- based on https://github.com/seaneking/postcss-responsive-type
    html ? ("font-size" -: "calc(12px + 9 * ((100vw - 600px) / 1024))")
    pageParts ? width (pct 95)
    "#author-link" ? do
      fontSize $ rem 1
      marginTop $ rem 0
      marginBottom $ rem (-1)
      transition "opacity" (sec 0.3) easeInOut (sec 0)
      opacity 0
      focus & opacity 1
    ".site-content" ? do
      "-webkit-flex-direction" -: "row"
      "flex-direction" -: "row"
    ".site-author" ? do
      flexValue 1
      orderValue 1
      marginRight $ em 2
    ".site-main" ? do
      flexValue 3
      orderValue 10
      overflow hidden
      display block
    ".index-main" ? do
      flexValue 1
      overflow hidden
    ".index-category" ? do
      sym2 padding nil $ em 1
    ".main-nav" ** (span <> a) ? do
      fontSize $ em 1.2
      sym margin $ em 0.7
    ".entry-main" ? do
      sym padding $ em 1
    ".entry-footer" ? do
      fontSize $ pct 95

  query M.screen [M.minWidth $ em 65] $ do
    ".index-main" ? do
      "-webkit-flex-direction" -: "row"
      "flex-direction" -: "row"

  query M.screen [M.minWidth $ em 80] $ do
    html ? ("font-size" -: "calc(12px + 9 * ((60em + 25vw - 600px) / 1024))")
    pageParts ? do
      width $ pct 90
      maxWidth $ em 80
    ".site-header" ? sym2 padding (em 3) nil
    ".site-author" ? do
      maxWidth $ em 18
      marginRight $ em 2

  query M.screen [M.minWidth $ em 105] $ do
    ".note-in-list" ** ".permalink" ? do
      display inline

pageParts, entryParts ∷ Selector
pageParts = ".site-header" <> ".site-content" <> ".site-footer"
entryParts = ".entry-header" <> ".entry-content" <> ".entry-footer" <> ".entry-responses"

grayedOut ∷ Css
grayedOut = color "#aaa"

fSize, flexValue, orderValue ∷ Integer → Css
fSize x = do
  fontSize . px  $ x
  fontSize . rem $ fromIntegral x / 18.0
  marginBottom . px . round $ 29.7 / fromIntegral x * (18.0 ∷ Double)
  marginBottom . rem $ 29.7 / fromIntegral x

flexValue x = do
  "-webkit-flex" -: tshow x
  "flex" -: tshow x

orderValue x = do
  "-webkit-order" -: tshow x
  "order" -: tshow x

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
