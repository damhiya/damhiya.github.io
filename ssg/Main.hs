{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String
import Data.Default
import Data.Either
import Data.List
import           Control.Concurrent            (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.LVar                     as L
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Encoding as T
import           Dhall                         (FromDhall, Generic, auto,
                                                inputFile)
import           Ema                           (Ema (..))
import qualified Ema                           as E
import qualified          Text.Blaze.Html.Renderer.Utf8 as H
import qualified          Text.Hamlet                   as H
import qualified Text.Cassius as C
import qualified System.FSNotify as FS
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Highlighting as P
import Network.URI

-- Model and it's FromDhall instance

data Format = MarkDown | HTML
  deriving (Show, Generic)

data Post = Post { identifier :: Text
                 , content    :: Text
                 , format     :: Format
                 }
  deriving (Show, Generic)

data Model = Model { syntaxHighlight :: Text
                   , posts :: [Post]
                   }
  deriving (Show, Generic)

instance FromDhall Format
instance FromDhall Post
instance FromDhall Model

-- Route
data Route
  = RIndex
  | RSiteMap
  | RMainCss
  | RSyntaxCss
  | RPost Text
  deriving Show

-- Ema instance

dropBack :: Int -> [a] -> [a]
dropBack n xs = take (length xs - n) xs

takeBack :: Int -> [a] -> [a]
takeBack n xs = drop (length xs - n) xs

instance Ema Model Route where
  encodeRoute _ = \case
    RIndex     -> "index.html"
    RSiteMap   -> "sitemap.txt"
    RMainCss   -> "css/main.css"
    RSyntaxCss -> "css/syntax.css"
    (RPost i)  -> "posts/" ++ T.unpack i ++ ".html"
  decodeRoute model = \case
    "index.html"  -> Just RIndex
    "sitemap.txt" -> Just RSiteMap
    "css/main.css"    -> Just RMainCss
    "css/syntax.css"  -> Just RSyntaxCss
    path | ("posts/" `isPrefixOf` path) && (".html" `isSuffixOf` path)
      -> let ident' = T.pack . dropBack 5 . drop 6 $ path in
           if any ((== ident') . identifier) (posts model)
             then Just (RPost ident')
             else Nothing
    _ -> Nothing
  allRoutes model = [ RIndex, RSiteMap, RMainCss, RSyntaxCss ]
                 ++ [ RPost (identifier post) | post <- posts model ]

-- render

markdownToHtml :: T.Text -> H.Html
markdownToHtml content = flip fromRight (P.runPure $ read content >>= write)
  [H.shamlet|
    markdownToHtmlError!
  |]
  where
    ropt = def
      { P.readerExtensions = P.githubMarkdownExtensions
                          <> P.extensionsFromList [ P.Ext_tex_math_single_backslash ]
      }
    wopt = def
      { P.writerHighlightStyle = Just P.tango
      , P.writerHTMLMathMethod = P.KaTeX P.defaultKaTeXURL
      }
    read  = P.readMarkdown ropt
    write = P.writeHtml5 wopt

render :: Model -> Route -> E.Asset ByteString
render model = \case
  RSiteMap -> E.AssetGenerated E.Other . fromString . unlines $
    [ "https://damhiya.github.io/" ++
      case route of
        RIndex     -> "index.html"
        RSiteMap   -> "sitemap.txt"
        RMainCss   -> "css/main.css"
        RSyntaxCss -> "css/syntax.css"
        (RPost i)  -> "posts/" ++ escape (T.unpack i) ++ ".html"
    | route <- [ RIndex ] ++ [ RPost (identifier post) | post <- posts model ]
    ]

  RMainCss   -> E.AssetGenerated E.Other . T.encodeUtf8 . C.renderCss $ 
    [C.cassius|
      body
        margin: 0
        font-family: sans-serif
      a
        text-decoration: none
      a:hover
        text-decoration: underline
      #sidebar
        box-sizing: border-box
        position: fixed
        left: 0
        width: 18rem
        height: 100%
        padding: 30px
        background: #0C0C5A
      #sidebar > a
        color: white
      main
        margin-left: 18rem
        max-width: 800px
        padding: 50px
    |] renderUrl

  RSyntaxCss -> E.AssetGenerated E.Other . fromString . T.unpack $ syntaxHighlight model

  RIndex -> E.AssetGenerated E.Html . H.renderHtml $
    [H.hamlet|
      $doctype 5
      <html>
        <head>
          <meta charset="utf-8">
          <title> damhiya's blog
          <link rel="stylesheet" href=@{ RMainCss }>
          <meta name="google-site-verification" content="D7Di9xyWnUvKT42Zw-idSUh7pgZz2OpC3xY97jd_UII">
        <body>
          #{ sidebar }
          <main>
            <a href="https://github.com/damhiya"> 깃헙 프로필
            <h2> Posts
            <ul>
              #{ links }
    |] renderUrl

  RPost ident' -> E.AssetGenerated E.Html . H.renderHtml $
    case find ((== ident') . identifier) (posts model) of
      Nothing ->
        [H.shamlet|
          $doctype 5
          <html>
            <head>
            <body>
              Post not found!
        |]
      Just post ->
        [H.hamlet|
          $doctype 5
          <html>
            <head>
              <meta charset="utf-8">
              <title> #{ identifier post }
              #{ katex }
              <link rel="stylesheet" href=@{ RMainCss }>
              <link rel="stylesheet" href=@{ RSyntaxCss }>
            <body>
              #{ sidebar }
              <main>
                #{ markdownToHtml (content post) }
                #{ utterances }
        |] renderUrl
  where
    escape = escapeURIString isUnescapedInURIComponent
    renderUrl r _ = "/" ++ encodeRoute model (r :: Route)
    sidebar =
      [H.hamlet|
        <div id="sidebar">
          <a href=@{ RIndex }> Home
      |] renderUrl
    links = [ [H.hamlet|
                <li>
                  <a href=@{ RPost (identifier post) }>
                    #{ identifier post }
              |] renderUrl
            | post <- posts model
            ]
    katex =
      [H.hamlet|
        <link rel="stylesheet"
              href="https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.css"
              integrity="sha384-RZU/ijkSsFbcmivfdRBQDtwuwVqK7GMOw6IMvKyeWL2K5UAlyp6WonmB8m7Jd0Hn"
              crossorigin="anonymous">
        <script defer
                src="https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.js"
                integrity="sha384-pK1WpvzWVBQiP0/GjnvRxV4mOb0oxFuyRxJlk6vVw146n3egcN5C925NCP7a7BY8"
                crossorigin="anonymous">
        <script>
          document.addEventListener("DOMContentLoaded", () => {
            [...document.getElementsByClassName("math display")].forEach( element =>
              katex.render(element.textContent, element, {throwOnError: false, displayMode: true})
            );
            [...document.getElementsByClassName("math inline")].forEach( element =>
              katex.render(element.textContent, element, {throwOnError: false, displayMode: false})
            );
          });
      |] renderUrl
    utterances =
      [H.hamlet|
        <script src="https://utteranc.es/client.js"
          repo="damhiya/damhiya.github.io"
          issue-term="pathname"
          theme="github-light"
          crossorigin="anonymous"
          async>
      |] renderUrl

-- main
run :: FilePath -> IO ()
run dir = do
  void $ FS.withManagerConf conf $ \manager -> do
    E.runEma (const render) $ \_ vmodel -> liftIO $ do
      readModel >>= L.set vmodel
      _ <- FS.watchTree manager dir (const True) $ \event -> do
        readModel >>= L.set vmodel
      threadDelay maxBound
  where
    conf :: FS.WatchConfig
    conf = FS.defaultConfig { FS.confDebounce = FS.Debounce 0.1 }

    readModel :: IO Model
    readModel = inputFile auto (dir ++ "/index.dhall")

main :: IO ()
main = run "./data"
