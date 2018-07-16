{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables             #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson                           hiding (json)
import           Data.Aeson.Extra                     hiding (json, value)
import           Data.Int                             (Int64)
import           Data.List
import           Data.Semigroup
import           Data.String (fromString)
import           Data.Text (Text,toLower)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Text.Casing hiding (Identifier)
import           GHC.Generics
import           Network.HTTP.Types.Status
import           Safe
import           Web.Spock                            hiding (head)
import Text.Regex
import Union
import Data.String.Conversions hiding ((<>))


{-todo use Text to build QualifiedIdentifier and Identifier types to use the existing library substitution-}
newtype TableName a = TableName {unTableName :: Identifier}
newtype ValueList a = ValueList {fromValueList :: [Text]} deriving (Show,Eq)
instance ToRow ( ValueList a ) where
  toRow (ValueList a) = (toField . Identifier . cs) <$> a

instance ToField (TableName a) where
  toField = toField . unTableName

removeTableName :: String -> String -> String
removeTableName tn cn =
  subRegex (mkRegexWithOpts ("^" ++ tn) True False) cn ""

class (Show (ColumnType a), Enum (ColumnType a), Eq (ColumnType a), ToRow a, FromRow a) => Table a where
  tableName :: TableName a
  data ColumnType a :: *
  {-columnName :: ColumnType a -> String-}
  valueList :: ValueList a
  valueList = colsToValueList [toEnum 0 ..] 

  insertValueList :: ValueList a
  insertValueList = colsToValueList (filter (\x->x /= primaryKey) [toEnum 0 ..])

  colsToValueList :: [ColumnType a] -> ValueList a
  colsToValueList a = ValueList $ (columnName) <$> a

  columnName :: ColumnType a -> Text
  columnName = toLower . cs . (removeTableName $ cs (fromIdentifier $ unTableName (tableName :: TableName a))) . show

  primaryKey :: ColumnType a
  primaryKey = toEnum 0

newtype Key a = Key {unKey :: Int64} deriving (Generic,Show)
instance FromField (Key a) where
  fromField f mdata = Key <$> fromField f mdata
instance ToField (Key a) where
  toField k = toField $ unKey k
instance ToJSON (Key a) where
  toJSON = toJSON . unKey
instance FromJSON (Key a) where
  parseJSON a = Key <$> parseJSON a

data Record a = Record {key :: Key a, value :: a} deriving (Generic)
instance (ToJSON a) => ToJSON (Record a) where
  toJSON a = lodashMerge (object [ "id" .= unKey (key a) ]) (toJSON  (value a))
instance (FromRow a) => FromRow (Record a) where
  fromRow = Record <$> field <*> fromRow

data Tag = Tag {name :: String} deriving (Show, Generic)
{-tagName t = name (t :: Tag)-}
instance ToJSON Tag
instance FromJSON Tag
instance FromRow Tag where
  fromRow = Tag <$> field
instance ToRow Tag where
  toRow t = [toField . (name :: Tag -> String)] <*> [t]
instance Table Tag where
  tableName = TableName $ Identifier "tags"
  data ColumnType Tag = TagsId | TagsName deriving (Enum,Show,Eq)
  valueList = ValueList ["name"]

data Song = Song {name :: String} deriving (Show, Generic)
{-songName s = name (s :: Song)-}
instance ToJSON Song
instance FromJSON Song
instance FromRow Song where
  fromRow = Song <$> field
instance ToRow Song where
  toRow t = [toField . (name :: Song -> String)] <*> [t]
instance Table Song where
  tableName = TableName $ Identifier "songs"
  data ColumnType Song = SongsId | SongsName deriving (Enum,Show,Eq)

data SongTag = SongTag {songId :: Key Song, tagId :: Key Tag} deriving (Show, Generic)
instance ToJSON SongTag
instance FromJSON SongTag
instance FromRow SongTag
instance ToRow SongTag where
  toRow t = [toField . songId, toField . tagId] <*> [t]
instance Table SongTag where
  tableName = TableName $ Identifier "songtags"
  data ColumnType SongTag = SongTagsId | SongTagsSongId | SongTagsTagId deriving (Enum,Show,Eq)

instance (Table a) => ToField (ColumnType a) where
  toField a = let tn = fromIdentifier $ unTableName (tableName :: TableName a)
              in toField $ QualifiedIdentifier (Just tn) (columnName a)
instance (ToField a, ToField b) => ToField (Either a b) where
  toField (Left a) = toField a
  toField (Right a) = toField a

{-class (Table a, Table b) => Association a b where-}
  {-columns :: (ColumnType a, ColumnType b)-}

{-instance Association SongTag Song where-}
  {-columns = (SongTagsSongId, SongsId)-}

{-joinMany :: (Table a,Table b) => Key a -> ColumType b -> ConstructedQuery [Record b]-}
{-joinMany key col =  joinQuery tableName key (columName col)-}

{-joinQuery :: (Table a, Table b) => TableName b -> Key a -> ColumnName b-}
{-joinQuery (TableName name) key colname = ConstructedQuery ""-}

data Constraint a = Equal a a | Less a a | GreaterOrEqual a a | And (Constraint a) (Constraint a)

(=.) :: (Setable a u, Setable b u) => a -> b -> Constraint u
a =. b = Equal (set a) (set b)

(<.) :: (Setable a u, Setable b u) => a -> b -> Constraint u
a <. b = Less (set a) (set b)

(>=.) :: (Setable a u, Setable b u) => a -> b -> Constraint u
a >=. b = GreaterOrEqual (set a) (set b)

class ToQuery a where
  toQuery :: a -> ConstructedQuery b
instance (ToField a) => ToQuery (Constraint a) where
  toQuery (Equal a b) = constructedQuery ("? = ?") (a,b)
  toQuery (Less a b) = constructedQuery ("? < ?") (a,b)
  toQuery (GreaterOrEqual a b) = constructedQuery ("? >= ?") (a,b)
  toQuery (And a b) = (toQuery a) <> (ConstructedQuery " and " []) <> (toQuery b)

queryHeader
  :: (TableName a, TableName b, TableName c) -> ConstructedQuery (Record a :. Record b :. Record c)
queryHeader (a, b, c) =
  ConstructedQuery "select * from ?, ?, ?" $ [toField a, toField b, toField c]

type ExtraTypes = Int + Key Song + Key Tag + Key SongTag

buildConstraintQuery
  :: (Table a, Table b, Table c)
  => Constraint
       ((ColumnType a) + (ColumnType b) + (ColumnType c) + ExtraTypes)
  -> ConstructedQuery (Record a :. Record b :. Record c)
buildConstraintQuery = toQuery

build
  :: (Table a, Table b, Table c)
  => Constraint
       ((ColumnType a) + (ColumnType b) + (ColumnType c) + ExtraTypes)
  -> ConstructedQuery (Record a :. Record b :. Record c)
build constraints =
  queryHeader (tableName, tableName, tableName) <> ConstructedQuery " where " [] <>(toQuery constraints)

executeQuery :: (FromRow a, HasSpock m, SpockConn m ~ Connection) => ConstructedQuery a -> m [a]
executeQuery q = runQuery $ \conn -> query conn qs qv
  where ConstructedQuery qs qv = q

find
  :: (Table a, FromRow a, HasSpock m, SpockConn m ~ Connection)
  => Key a
  -> m (Maybe (Record a))
find k = runQuery $ findQuery tableName k
findQuery
  :: (FromRow a) => TableName a -> Key a -> Connection -> IO (Maybe (Record a))
findQuery (TableName n) (Key k) conn =
  headMay <$> query conn "select * from ? where id = ?;" (n, k)

find404
  :: ( Table a
     , FromRow a
     , SpockConn (ActionCtxT ctx m) ~ Connection
     , Monad m
     , HasSpock m
     , HasSpock (ActionCtxT ctx m)
     , MonadIO m
     )
  => Key a
  -> ActionCtxT ctx m (Record a)
find404 a = do
  rec <- Lib.find a
  val <- guardNotFound rec
  return val

getAll
  :: (Table a, FromRow a, HasSpock m, SpockConn m ~ Connection)
  => m ([Record a])
getAll = runQuery $ getFromTable tableName

{-we need the second layer of function here to make the type inference find the-}
{-right "tableName function in getAll"-}
getFromTable :: (FromRow a) => TableName a -> Connection -> IO ([Record a])
getFromTable (TableName n) conn = query conn "select * from ?;" (Only n)

{-Generic insert logic-}
insertRecord
  :: (Table a, HasSpock m, SpockConn m ~ Connection)
  => a
  -> m (Record a)
insertRecord = runQuery . insertElement

insertElement
  :: (Table a, ToRow a, FromRow a) => a -> Connection -> IO (Record a)
insertElement e = executeInsertQuery (insertQuery tableName insertValueList e)

data ConstructedQuery a = ConstructedQuery Query [Action] deriving (Show)
constructedQuery :: (ToRow a) => String -> a -> ConstructedQuery b
constructedQuery t a = ConstructedQuery (fromString t) (toRow a)
instance Semigroup (ConstructedQuery a) where
  (ConstructedQuery q1 a1) <> (ConstructedQuery q2 a2) = ConstructedQuery (q1 <> q2) (a1 ++ a2)
instance Eq (ConstructedQuery a) where
  (==) a b = (show a) == (show b)

{-joinQueries ()-}

insertQuery
  :: (Table a) => TableName a -> ValueList a -> a -> ConstructedQuery a
insertQuery (TableName n) values a =
  let paramString = intercalate "," $ replicate (length $ fromValueList values) "?"
  in  ConstructedQuery
        (  fromString
        $  "insert into ? ("
        ++ paramString
        ++ ") values ("
        ++ paramString
        ++ ") returning *;"
        )
        ([toField n] ++ (toRow values) ++ (toRow a))

executeInsertQuery
  :: (ToRow a, FromRow a) => ConstructedQuery a -> Connection -> IO (Record a)
executeInsertQuery (ConstructedQuery q e) conn = head <$> query conn q e

{-Generic delete logic-}
deleteRecord
  :: (Table a, HasSpock m, SpockConn m ~ Connection) => Key a -> m (Int64)
deleteRecord = runQuery . deleteElement

deleteElement :: (Table a) => Key a -> Connection -> IO (Int64)
deleteElement k = executeDeleteQuery (deleteQuery tableName k)

deleteQuery :: TableName a -> Key a -> ConstructedQuery a
deleteQuery (TableName n) k =
  ConstructedQuery "delete from ? where id = ?;" [toField n, toField k]

executeDeleteQuery :: ConstructedQuery a -> Connection -> IO (Int64)
executeDeleteQuery (ConstructedQuery q vars) conn = execute conn q vars

{-addTag' :: Tag -> Connection -> IO (Record Tag)-}
{-addTag' t conn = head <$> query conn "insert into tags (name) values (?) returning *" t-}

{-{-This relies on type inference to work so it won't compile unless used in the spock monad-}-}
{-addTag = runQuery . addTag'-}

{-getTags' :: Connection -> IO ([Record Tag])-}
{-getTags' conn = query_ conn "select * from tags;"-}
{-getTags = runQuery getTags'-}

guardNotFound :: MonadIO m => Maybe (Record a) -> ActionCtxT ctx m (Record a)
guardNotFound Nothing = do
  setStatus status404
  text "Not found"
guardNotFound (Just a) = return a
