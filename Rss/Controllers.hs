{-# LANGUAGE OverloadedStrings #-}
module Rss.Controllers where

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad
import Debug.Trace

import Hails.HttpServer hiding (Query)
import Hails.Web
import Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import Hails.Web.Frank
import Hails.Web.Controller
import Hails.Data.Hson
import Hails.Web.User
import Hails.Database
import Hails.Database.Structured hiding (findAll)
import qualified Hails.Database.Structured as D
import LIO
import LIO.DCLabel
import Data.Maybe
import Data.Time.Clock
import Data.List.Split
import Data.Aeson (decode, encode, toJSON)
import Text.Blaze.Renderer.Text
import Text.Blaze.Html5 hiding (Tag, map, head, select, label)

import qualified Web.Simple.Responses as W

import Rss.Policy
import Rss.Models
import Rss.Views

server :: Application
server = mkRouter $ do
 
  get "/" $ withUserOrDoAuth $ \user -> do
    priv <- appPriv
    respond $ respondHtml "Home" $ displayHomePage user