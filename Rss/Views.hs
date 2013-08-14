{-# LANGUAGE OverloadedStrings #-}
module Rss.Views where

import Prelude hiding (div, span, id, lookup, head)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad
import Debug.Trace

import Hails.HttpServer hiding (Query)
import Hails.Web hiding (body)
import Hails.Web.Frank
import Hails.Web.Controller hiding (body)
import Hails.Data.Hson
import Hails.Web.User
import LIO hiding (label)
import LIO.DCLabel
import Data.Maybe
import Data.List hiding (head, span)
import Data.Ord
import Data.Bson (timestamp)
import Data.Time.Clock
import Data.Time.LocalTime

import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR
import           Text.Regex

import Rss.Models

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    stylesheet "/static/css/bootstrap.css"
    title ctitle
  body ! id "body" $ do
    div ! class_ "navbar navbar-fixed-top navbar-inverse" ! id "page-nav" $ do
      div ! class_ "navbar-inner" $ do
        div ! class_ "container-fluid" $ do
          a ! href "/" ! class_ "brand" $ "Rss Reader"
    content

-- Projects -----

displayHomePage :: UserName -> Html
displayHomePage user = do
  div ! class_ "welcome" $ do
   h2 $ toHtml $ "Welcome " ++ T.unpack user
  div ! class_ "user-input" $ do
    form ! class_ "removenotif" ! action "/feed/add" ! method "post" $ do
      label ! for "feed" $ "feed url: "
      input ! type_ "text" ! name "feed" ! value ""
      button ! class_ "removeNotif" ! type_ "submit" $ "add feed"
   