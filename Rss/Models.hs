{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Rss.Models where
--exported models ^ 
import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8

import Control.Monad
import Debug.Trace

import Hails.HttpServer hiding (Query)
import Hails.Web
import Hails.Web.Frank
import Hails.Web.Controller
import Hails.Data.Hson
import Hails.Web.User
import Hails.Database
import Hails.Database.Structured
import LIO
import LIO.DCLabel
import Data.Maybe
import Data.Time.Clock
import Data.Typeable

import Rss.Policy


data User = User {
  userId :: Maybe ObjectId,
  userName :: UserName,
  userFeeds :: [Feed],
} deriving (Show, Eq, Typeable)

instance ToJSON User where
  toJSON (User id name feeds) =
    object [ "_id"       .= (show $ fromJust id)
           , "name"      .= name
           , "feeds"    .= notifs
           ]

instance DCRecord User where
  fromDocument doc = trace "fromDoc user" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    feeds <- lookup "feeds" doc
    trace "returning user" $ return User { userId = uid
                , userName = name
                , userFeeds = feeds}

  toDocument u = trace "toDoc user" $
    [ "_id"  -: userId u
    , "name" -: userName u
    , "feeds" -: userFeeds u
    ]

  recordCollection _ = "users"

data Feed = Feed {
  feedId :: Maybe ObjectId,
  feedTitle :: String,
  feedUrl :: String,
  feedEntries :: [Entry]
} deriving (Show, Eq, Typeable)


instance ToJSON Feed where
  toJSON (Feed id title url entries) =
    object [ "_id"       .= (show $ fromJust id)
           , "title"      .= title
           , "url"        .= url  
           , "entries"    .= entries
           ]

instance DCRecord Feed where
  fromDocument doc = trace "fromDoc feed" $ do
    let uid = lookupObjIdh "_id" doc
    name <- lookup "name" doc
    feeds <- lookup "feeds" doc
    url <- lookup "url" doc
    trace "returning feed" $ return Feed { feedId = uid
                , feedName = name
                , feedUrl = url
                , feedFeeds = feeds}

  toDocument f = trace "toDoc feed" $
    [ "_id"  -: feedId f
    , "title" -: feedTitle f
    , "url" -: feedUrl f
    , "entries" -: feedEntries f
    ]

  recordCollection _ = "feeds"

 data Entry = Entry {
  entryId :: Maybe ObjectId,
  entryTitle :: String,
  entryUrl :: String,
  entryAuthor :: String,
  entrySummary :: String,
  entryContent :: String,
  entryTime :: String
} deriving (Show, Eq, Typeable)


instance ToJSON Entry where
  toJSON (Entry id title url author summary content time) =
    object [ "_id"       .= (show $ fromJust id)
           , "title"      .= title
           , "url"        .= url  
           , "author"    .= author
           , "summary"   .= summary
           , "content"   .= content
           , "time"      .= time
           ]

instance DCRecord Entry where
  fromDocument doc = trace "fromDoc entry" $ do
    let uid = lookupObjIdh "_id" doc
    title <- lookup "title" doc
    author <- lookup "author" doc
    url <- lookup "url" doc
    summary <- lookup "summary" doc
    content <- lookup "content" doc
    time <- lookup "time" doc 
    trace "returning entry" $ return Entry { entryId = uid
                , entryTitle = name
                , entryUrl = url
                , entryAuthor = author
                , entrySummary = summary
                , entryContent = content
                , entryTime = time}

  toDocument e = trace "toDoc entry" $
    [ "_id"  -: entryId e
    , "title" -: entryTitle e
    , "url" -: entryUrl e
    , "author" -: entryAuthor e
    , "summary" -: entrySummary e
    , "content" -: entryContent e
    , "time" -: entryTime e
    ]

  recordCollection _ = "entries"
