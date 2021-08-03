{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Control.Monad
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Text.Cassius
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

data PostFormat = MarkDown deriving Show
type PostInfo = (Int, String, PostFormat)

data Route = Index | MainCss | SyntaxCss | Post PostInfo

extension :: RE Char PostFormat
extension = string "md" *> pure MarkDown

postInfo :: RE Char PostInfo
postInfo = (,,) <$> decimal <* sym '-' <*> few anySym <* sym '.' <*> extension

renderUrl :: T.Text -> Route -> [(T.Text, T.Text)] -> T.Text
renderUrl root Index     _ = root <> "/index.html"
renderUrl root MainCss   _ = root <> "/css/main.css"
renderUrl root SyntaxCss _ = root <> "/css/syntax.css"
renderUrl root (Post (id, title, format)) _ = root <> "/posts/" <> T.pack (title ++ ".html")

katex :: Html
katex =
  [hamlet|
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.css" integrity="sha384-RZU/ijkSsFbcmivfdRBQDtwuwVqK7GMOw6IMvKyeWL2K5UAlyp6WonmB8m7Jd0Hn" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.js" integrity="sha384-pK1WpvzWVBQiP0/GjnvRxV4mOb0oxFuyRxJlk6vVw146n3egcN5C925NCP7a7BY8" crossorigin="anonymous">
    <script>
      document.addEventListener("DOMContentLoaded", () => {
        [...document.getElementsByClassName("math display")].forEach( element =>
          katex.render(element.textContent, element, {throwOnError: false, displayMode: true})
        );
        [...document.getElementsByClassName("math inline")].forEach( element =>
          katex.render(element.textContent, element, {throwOnError: false, displayMode: false})
        );
      });
  |] (renderUrl "")

postTemplate :: String -> Html -> Html
postTemplate title content =
  [hamlet|
    $doctype 5
    <html>
      <head>
        #{ katex }
        <link rel=stylesheet href=@{ MainCss }>
        <link rel=stylesheet href=@{ SyntaxCss }>
        <title> #{ title }
      <body>
        <div class=page-wrapper>
          #{ content }
          <script src="https://utteranc.es/client.js"
            repo="damhiya/damhiya.github.io"
            issue-term="pathname"
            theme="github-light"
            crossorigin="anonymous"
            async>
  |] (renderUrl "")
 
indexTemplate :: [PostInfo] -> Html
indexTemplate infos =
  [hamlet|
    $doctype 5
    <html>
      <head>
        <meta name="google-site-verification" content="D7Di9xyWnUvKT42Zw-idSUh7pgZz2OpC3xY97jd_UII">
        <link rel=stylesheet href=@{ MainCss }>
        <title> damhiya's blog
      <body>
        <div class=page-wrapper>
          <a href="https://github.com/damhiya"> 깃헙 프로필
          <h2> Posts
          <ul>
            #{ mconcat (map listElementTemplate infos) }
  |] (renderUrl "")
  -- TODO: sort on id of posts
  where
    listElementTemplate :: PostInfo -> Html
    listElementTemplate (id, title, format) =
      [hamlet|
        <li>
          <a href=@{ Post (id, title, format) }> #{ title }
      |] (renderUrl "")

mainCss :: Css
mainCss =
  [cassius|
    body
      font-family: sans-serif
    .page-wrapper
      max-width: 1000px
      margin: auto
      padding: 30px
  |] (renderUrl "")

convertToHtml :: PostFormat -> T.Text -> Html
convertToHtml format = \content -> case runPure (reader format content >>= writer) of
  Left _ -> error "conversion failed"
  Right r -> r
  where
    reader MarkDown = readMarkdown def {readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_tex_math_single_backslash]}
    writer = writeHtml5 def {writerHighlightStyle = Just tango, writerHTMLMathMethod = KaTeX defaultKaTeXURL}

srcRoot :: T.Text
srcRoot = "./src"

outputRoot :: T.Text
outputRoot = "./docs"

main :: IO ()
main = do
  fnames <- listDirectory (T.unpack srcRoot ++ "/posts")
  let postInfos = map (match postInfo) fnames

  forM_ (zip fnames postInfos) $ \case
    (fname, Nothing) -> putStrLn ("wrong file name! :" ++ fname)
    (fname, Just (id, title, format)) -> do
      content <- T.readFile (T.unpack srcRoot ++ "/posts/" ++ fname)
      let content' = postTemplate title (convertToHtml format content)
      TL.writeFile (T.unpack (renderUrl outputRoot (Post (id, title, format)) [])) (renderHtml content')

  TL.writeFile (T.unpack $ renderUrl outputRoot Index     []) (renderHtml . indexTemplate $ catMaybes postInfos)
  TL.writeFile (T.unpack $ renderUrl outputRoot MainCss   []) (renderCss mainCss)
  writeFile    (T.unpack $ renderUrl outputRoot SyntaxCss []) (styleToCss tango)
