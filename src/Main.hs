{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Concurrent            (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy
import qualified Data.LVar                     as L
import           Data.Some
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Dhall                         (FromDhall, Generic, auto,
                                                inputFile)
import           Ema                           (Ema (..))
import qualified Ema                           as E
import qualified Ema.CLI                       as E
import qualified          Text.Blaze.Html.Renderer.Utf8 as H
import qualified          Text.Hamlet                   as H

-- Model and it's FromDhall instance
data Format = MarkDown | HTML
  deriving (Show, Generic)

data Post = Post { title   :: Text
                 , content :: Text
                 , format  :: Format
                 }
  deriving (Show, Generic)

type Model = [Post]

instance FromDhall Format
instance FromDhall Post

-- Route
data Route
  = RIndex
  | RPost Text
  deriving Show

-- Ema instance
instance Ema Model Route where
  encodeRoute _model RIndex = "index.html"
  encodeRoute _model _ = "index.html"
  decodeRoute _model "index.html" = Just RIndex
  decodeRoute _model _            = Nothing
  allRoutes _model = undefined

-- render
render' :: Model -> Route -> H.Html
render' model route =
  [H.shamlet|
    $doctype 5
    <html>
      <head>
      <body>
        hello!!
  |]
  
render :: Some E.Action -> Model -> Route -> E.Asset ByteString
render _ model route = E.AssetGenerated E.Html . H.renderHtml $ render' model route

-- main
main :: IO ()
main = do
  posts <- inputFile auto "./data/index.dhall" :: IO Model
  void $ E.runEma render $ \_act model -> do
    L.set model $ []
    liftIO $ threadDelay maxBound

