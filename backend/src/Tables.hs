{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}


module Tables where

import           Database.PostgreSQL.Simple.FromRow
{-import           Database.PostgreSQL.Simple.FromField-}
{-import           Database.PostgreSQL.Simple.ToField-}
import           Database.PostgreSQL.Simple.ToRow
import           Data.Aeson                           hiding (json)
import           GHC.Generics
import           Data.Text (Text)
import Database.PostgreSQL.Simple.Types
import Lib
import Data.Time
import Web.Users.Types

data Card = Card {
    question :: Text,
    answer :: Text,
    lastAnsweredAt :: UTCTime,
    dueAt :: UTCTime,
    categoryId :: Key Category
} deriving (Show, Generic)
instance ToJSON Card
instance FromJSON Card
instance FromRow Card
instance ToRow Card
instance Table Card where
  tableName = TableName $ Identifier "cards"
  valueList = cardIdC <! cardQuestionC <: cardAnswerC <: cardLastAnsweredAtC <: cardDueAtC <: cardCategoryIdC

cardIdC = Column "id" :: Column Card (Key Card)
cardQuestionC = Column "question" :: Column Card (Text)
cardAnswerC = Column "answer" :: Column Card (Text)
cardLastAnsweredAtC = Column "last_answered_at" :: Column Card (UTCTime)
cardDueAtC = Column "due_at" :: Column Card (UTCTime)
cardCategoryIdC = Column "category_id" :: Column Card (Key Category)


data Category = Category {
    name :: Text
} deriving (Show, Generic)
instance ToJSON Category
instance FromJSON Category
instance FromRow Category
instance ToRow Category
instance Table Category where
  tableName = TableName $ Identifier "categories"
  valueList = categoryIdC <! categoryNameC

categoryIdC = Column "id" :: Column Category (Key Card)
categoryNameC = Column "name" :: Column Category Text

