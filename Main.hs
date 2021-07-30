{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Control.Monad
import Data.Text
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Text.Pandoc
import Text.Pandoc.Highlighting

markdownToHtml :: Text -> Html
markdownToHtml str =
  case runPure (convert str) of
    Left _ -> error "conversion failed"
    Right r -> r
  where
    convert = readMarkdown def {readerExtensions = exts}
          >=> writeHtml5 def {writerHighlightStyle = Just tango}
      
    exts = githubMarkdownExtensions

main :: IO ()
main = do
  sample <- T.readFile "./src/sample.md"
  let
    index = [shamlet|
      $doctype 5
      <html>
        <head>
          <link rel=stylesheet href=syntax.css>
          <title> damhiya's blog (index)
        <body>
          <p> Some contents!
          #{ markdownToHtml sample }
    |]

  TL.writeFile "./docs/index.html" (renderHtml index)
  writeFile "./docs/syntax.css" (styleToCss tango)
