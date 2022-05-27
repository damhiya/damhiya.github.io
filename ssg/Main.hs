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
import           Dhall                         (FromDhall, Generic, auto,
                                                inputFile)
import           Ema                           (Ema (..))
import qualified Ema                           as E
import qualified          Text.Blaze.Html.Renderer.Utf8 as H
import qualified          Text.Hamlet                   as H
import qualified System.FSNotify as FS
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Highlighting as P
import qualified Network.URI as URI

-- Model and it's FromDhall instance

data Format = MarkDown | HTML
  deriving (Show, Generic)

data Post = Post { identifier :: Text
                 , title      :: Text
                 , content    :: Text
                 , format     :: Format
                 }
  deriving (Show, Generic)

data Model = Model { mainCss   :: Text
                   , syntaxCss :: Text
                   , posts     :: [Post]
                   , haskellTutorial :: [Post]
                   }
  deriving (Show, Generic)

instance FromDhall Format
instance FromDhall Post
instance FromDhall Model

-- Route
data PostCategory = Normal | HaskellTutorial deriving Show
data Route
  = RIndex
  | RSiteMap
  | RMainCss
  | RSyntaxCss
  | RPost PostCategory Text
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
    (RPost Normal i)  -> "posts/" ++ T.unpack i ++ ".html"
    (RPost HaskellTutorial i) -> "haskellTutorial/" ++ T.unpack i ++ ".html"
  decodeRoute model = \case
    "index.html"     -> Just RIndex
    "sitemap.txt"    -> Just RSiteMap
    "css/main.css"   -> Just RMainCss
    "css/syntax.css" -> Just RSyntaxCss
    path | ("posts/" `isPrefixOf` path) && (".html" `isSuffixOf` path)
      -> let ident' = T.pack . dropBack 5 . drop 6 $ path in
           if any ((== ident') . identifier) (posts model)
             then Just (RPost Normal ident')
             else Nothing
    path | ("haskellTutorial/" `isPrefixOf` path) && (".html" `isSuffixOf` path)
      -> let i = T.pack . dropBack 5 . drop 16 $ path in
           if any ((== i) . identifier) (haskellTutorial model)
             then Just (RPost HaskellTutorial i)
             else Nothing
    _ -> Nothing
  allRoutes model = [ RIndex, RSiteMap, RMainCss, RSyntaxCss ]
                 ++ [ RPost Normal (identifier p) | p <- posts model ]
                 ++ [ RPost HaskellTutorial (identifier p) | p <- haskellTutorial model ]

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
        (RPost Normal i)  -> "posts/" ++ escape (T.unpack i) ++ ".html"
        (RPost HaskellTutorial i) -> "haskellTutorial/" ++ escape (T.unpack i) ++ ".html"
    | route <- [ RIndex ]
            ++ [ RPost Normal (identifier p) | p <- posts model ]
            ++ [ RPost HaskellTutorial (identifier p) | p <- haskellTutorial model ]
    ]

  RMainCss   -> E.AssetGenerated E.Other . fromString . T.unpack $ mainCss model

  RSyntaxCss -> E.AssetGenerated E.Other . fromString . T.unpack $ syntaxCss model

  RIndex -> E.AssetGenerated E.Html . H.renderHtml $
    [H.hamlet|
      $doctype 5
      <html>
        <head>
          <meta charset="utf-8">
          <title> damhiya's blog
          <link rel="stylesheet" href=@{ RMainCss }>
          <meta name="google-site-verification" content="D7Di9xyWnUvKT42Zw-idSUh7pgZz2OpC3xY97jd_UII">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <body>
          #{ sidebar }
          <main>
            <a href="https://github.com/damhiya"> 깃헙 프로필
            <h2> Posts
            <ul> #{ links }
            <h2> Haskell tutorial
            <ul> #{ links' }
    |] renderUrl

  RPost Normal i -> E.AssetGenerated E.Html . H.renderHtml $
    post $ find ((== i) . identifier) (posts model)
  RPost HaskellTutorial i -> E.AssetGenerated E.Html . H.renderHtml $
    post $ find ((== i) . identifier) (haskellTutorial model)

  where
    escape = URI.escapeURIString URI.isUnescapedInURIComponent
    renderUrl r _ = "/" ++ encodeRoute model (r :: Route)
    sidebar =
      [H.hamlet|
        <div id="sidebar">
          <a href=@{ RIndex }> Home
      |] renderUrl
    links = [ [H.hamlet|
                <li>
                  <a href=@{ RPost Normal (identifier p) }>
                    #{ title p }
              |] renderUrl
            | p <- posts model
            ]
    links' = [ [H.hamlet|
                 <li>
                   <a href=@{ RPost HaskellTutorial (identifier p) }>
                     #{ title p }
               |] renderUrl
             | p <- haskellTutorial model
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

    post :: Maybe Post -> H.Html
    post Nothing =
      [H.shamlet|
        $doctype 5
        <html>
          <head>
          <body>
            Post not found!
      |]
    post (Just p) =
      [H.hamlet|
        $doctype 5
        <html>
          <head>
            <meta charset="utf-8">
            <title> #{ title p }
            #{ katex }
            <link rel="stylesheet" href=@{ RMainCss }>
            <link rel="stylesheet" href=@{ RSyntaxCss }>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <body>
            #{ sidebar }
            <main>
              #{ markdownToHtml (content p) }
              #{ utterances }
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
