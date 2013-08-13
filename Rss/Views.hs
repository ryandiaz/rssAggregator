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


-- Projects -----

displayHomePage :: User -> Html
displayHomePage user = do