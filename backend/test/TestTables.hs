{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}


module TestTables where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Data.Aeson                           hiding (json)
import           GHC.Generics
import           Data.Text (Text)
import Database.PostgreSQL.Simple.Types
import Lib

data Tag = Tag {name :: String} deriving (Show, Generic)
instance ToJSON Tag
instance FromJSON Tag
instance FromRow Tag where
  fromRow = Tag <$> field
instance ToRow Tag where
  toRow t = [toField . (name :: Tag -> String)] <*> [t]
instance Table Tag where
  tableName = TableName $ Identifier "tags"
  valueList = tagIdC <! tagNameC

tagIdC = Column "id" :: Column Tag (Key Tag)
tagNameC = Column "name" :: Column Tag (Text)

data Song = Song {name :: String} deriving (Show, Generic)
instance ToJSON Song
instance FromJSON Song
instance FromRow Song where
  fromRow = Song <$> field
instance ToRow Song where
  toRow t = [toField . (name :: Song -> String)] <*> [t]
instance Table Song where
  tableName = TableName $ Identifier "songs"
  valueList = songIdC <! songNameC

songIdC = Column "id" :: Column Song (Key Song)
songNameC = Column "name" :: Column Song Text

data SongTag = SongTag {songId :: Key Song, tagId :: Key Tag} deriving (Show, Generic)
instance ToJSON SongTag
instance FromJSON SongTag
instance FromRow SongTag
instance ToRow SongTag where
  toRow t = [toField . songId, toField . tagId] <*> [t]
instance Table SongTag where
  tableName = TableName $ Identifier "songtags"
  valueList = songTagIdC <! songTagSongIdC <: songTagTagIdC

songTagIdC = Column "id" :: Column SongTag (Key SongTag)
songTagSongIdC = Column "songid" :: Column SongTag (Key Song)
songTagTagIdC = Column "tagid" :: Column SongTag (Key Tag)
