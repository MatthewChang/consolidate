{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}


module Tables where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField
{-import           Database.PostgreSQL.Simple.ToField-}
import           Database.PostgreSQL.Simple.ToRow
import           Data.Aeson                           hiding (json)
import           GHC.Generics
import           Data.Text (Text)
import Database.PostgreSQL.Simple.Types
import Lib
import Data.Time

instance FromField NominalDiffTime where
  fromField f mdata = (realToFrac . secondsToDiffTime) <$> (fromField f mdata)

data Card = Card {
    question :: Text,
    answer :: Text,
    lastCorrectAt :: UTCTime,
    waitDuration :: NominalDiffTime,
    categoryId :: Key Category
} deriving (Show, Generic)
instance ToJSON Card
instance FromJSON Card
instance FromRow Card
instance ToRow Card
instance Table Card where
  tableName = TableName $ Identifier "cards"
  valueList = cardIdC <! cardQuestionC <: cardAnswerC <: cardLastCorrectAtC <: cardWaitDurationC <: cardCategoryIdC

cardIdC = Column "id" :: Column Card (Key Card)
cardQuestionC = Column "question" :: Column Card (Text)
cardAnswerC = Column "answer" :: Column Card (Text)
cardLastCorrectAtC = Column "last_correct_at" :: Column Card (UTCTime)
cardWaitDurationC = Column "wait_duration" :: Column Card (NominalDiffTime)
cardCategoryIdC = Column "category_id" :: Column Card (Key Category)


data Category = Category {
    name :: Text
} deriving (Show, Generic)
instance ToJSON Category
instance FromJSON Category
instance FromRow Category
instance ToRow Category
instance Table Category where
  tableName = TableName $ Identifier "tags"
  valueList = categoryIdC <! categoryNameC

categoryIdC = Column "id" :: Column Category (Key Card)
categoryNameC = Column "name" :: Column Category Text
