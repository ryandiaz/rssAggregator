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
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    script ! src "http://code.jquery.com/jquery-latest.min.js" $ ""
    script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
    script ! src "/static/js/jquery.form.js" $ ""
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/jquery.mockjax.js" $ ""
    script ! src "/static/js/jquery-1.6.4.js" $ ""
    script ! src "/static/js/jquery-1.7.2.js" $ ""
    script ! src "/static/js/jquery-1.8.3.js" $ ""
    script ! src "/static/js/jquery-1.9.0.js" $ ""
    script ! src "/static/js/jquery.validate.js" $ ""
    script ! src "/static/js/jquery.validate.min.js" $ ""
    script ! src "/static/js/additional-methods.js" $ ""
    script ! src "/static/js/additional-methods.min.js" $ ""
    script ! src "/static/js/tasks.js" $ ""
    script ! src "/static/js/newtask.js" $ ""
    script ! src "/static/js/user_select.js" $ ""
    content

-- Projects -----

displayHomePage :: UserName -> Html
displayHomePage user = do
  div ! class_ "welcome" $ do
   h2 $ toHtml $ "Welcome " ++ T.unpack user
   