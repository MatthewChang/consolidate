{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ConstraintKinds #-}


module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson                           hiding (json)
import           Data.Aeson.Extra                     hiding (json, value)
import           Data.Int                             (Int64)
import           Data.List
import           Data.Semigroup
import           Data.String (fromString)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import           GHC.Generics
import           Network.HTTP.Types.Status
import           Safe
import Servant
import Text.Regex
import Union
import Data.String.Conversions hiding ((<>))
import Debug.Trace (trace)


newtype TableName a = TableName {unTableName :: Identifier}
newtype ValueList a = ValueList {fromValueList :: [Text]} deriving (Show,Eq)
instance ToRow ( ValueList a ) where
  toRow (ValueList a) = (toField . Identifier . cs) <$> a

instance ToField (TableName a) where
  toField = toField . unTableName

removeTableName :: String -> String -> String
removeTableName tn cn = subRegex (mkRegexWithOpts ("^" ++ tn) True False) cn ""

data Column t ty = Column {fromColumn :: Text} deriving (Show)
instance (Table t) => ToField (Column t a) where
  toField a = let tn = fromIdentifier $ unTableName (tableName :: TableName t)
              in toField $ QualifiedIdentifier (Just tn) (fromColumn a)
data CheckedColumn t = CheckedColumn {toAction :: Action}
instance ToField (CheckedColumn a) where
  toField = toAction
(<:) :: ValueList a -> Column a ty -> ValueList a
ValueList l <: c = ValueList $ l ++ [fromColumn c]
(<!) :: Column a b -> Column a c -> ValueList a
a <! b = ValueList $ [fromColumn a, fromColumn b]

class (ToRow a, FromRow a) => Table a where
  tableName :: TableName a
  valueList :: ValueList a
  insertValueList :: ValueList a
  insertValueList = ValueList list' where
    (ValueList list) = valueList :: ValueList a
    list' = tail list

newtype Key a = Key {unKey :: Int64} deriving (Generic,Show)
instance FromField (Key a) where
  fromField f mdata = Key <$> fromField f mdata
instance ToField (Key a) where
  toField k = toField $ unKey k
instance ToJSON (Key a) where
  toJSON = toJSON . unKey
instance FromJSON (Key a) where
  parseJSON a = Key <$> parseJSON a

instance FromHttpApiData (Key a) where
    parseUrlPiece = t . parseUrlPiece where
      t (Right a) = Right $ Key a
      t (Left a) = Left a

data Record a = Record {key :: Key a, value :: a} deriving (Generic)
instance (ToJSON a) => ToJSON (Record a) where
  toJSON a = lodashMerge (object [ "id" .= unKey (key a) ]) (toJSON  (value a))
instance (FromRow a) => FromRow (Record a) where
  fromRow = Record <$> field <*> fromRow

type family ConstraintType a where
  ConstraintType (Column tb ty) = ty
  ConstraintType a = a

type family ConstraintTable a where
  ConstraintTable (Column tb ty) = tb
  ConstraintTable a = a

{-function for the constraint to use for the given types-}
{-for columns we check that they are in the give tables-}
{-anthing else just has to have to field defined-}
{-this allows us to use new types without needing -}
type family ConstraintFor a u where
  ConstraintFor (Column tb ty) u = Setable tb u
  ConstraintFor a _ = ToField a

data Constraint a = Equal Action Action | Less Action Action | GreaterOrEqual Action Action | And (Constraint a) (Constraint a)

(=.)
  :: ( ConstraintType a ~ ConstraintType b
     , ConstraintFor a u
     , ConstraintFor b u
     , ToField a
     , ToField b
     )
  => a
  -> b
  -> Constraint u
a =. b = Equal (toField a) (toField b)


(<.)
  :: ( ConstraintType a ~ ConstraintType b
     , ConstraintFor a u
     , ConstraintFor b u
     , ToField a
     , ToField b
     )
  => a
  -> b
  -> Constraint u
a <. b = Less (toField a) (toField b)

(>=.)
  :: ( ConstraintType a ~ ConstraintType b
     , ConstraintFor a u
     , ConstraintFor b u
     , ToField a
     , ToField b
     )
  => a
  -> b
  -> Constraint u
a >=. b = GreaterOrEqual (toField a) (toField b)

class ToQuery a where
  toQuery :: a -> ConstructedQuery b
instance ToQuery (Constraint a) where
  toQuery (Equal a b) = constructedQuery ("? = ?") (a,b)
  toQuery (Less a b) = constructedQuery ("? < ?") (a,b)
  toQuery (GreaterOrEqual a b) = constructedQuery ("? >= ?") (a,b)
  toQuery (And a b) = (toQuery a) <> (ConstructedQuery " and " []) <> (toQuery b)

queryHeader
  :: (TableName a, TableName b, TableName c)
  -> ConstructedQuery (Record a :. Record b :. Record c)
queryHeader (a, b, c) =
  ConstructedQuery "select * from ?, ?, ?" $ [toField a, toField b, toField c]


buildConstraintQuery
  :: (Table a, Table b, Table c)
  => Constraint (a + b + c)
  -> ConstructedQuery (Record a :. Record b :. Record c)
buildConstraintQuery = toQuery

build
  :: (Table a, Table b, Table c)
  => Constraint (a + b + c)
  -> ConstructedQuery (Record a :. Record b :. Record c)
build constraints =
  queryHeader (tableName, tableName, tableName)
    <> ConstructedQuery " where " []
    <> (toQuery constraints)

--executeQuery :: (FromRow a, HasSpock m, SpockConn m ~ Connection) => ConstructedQuery a -> m [a]
--executeQuery q = runQuery $ \conn -> query conn qs qv
  --where ConstructedQuery qs qv = q

runQuery :: (FromRow a) => ConstructedQuery a -> Connection -> IO [a]
runQuery (ConstructedQuery q r) c = query c q r

--find :: (Table a, FromRow a, HasSpock m, SpockConn m ~ Connection) => Key a -> m (Maybe (Record a)) find k = runQuery $ findQuery tableName k

findConnection
  :: (Table a, FromRow a) => Key a -> Connection -> IO (Maybe (Record a))
findConnection k = findQuery tableName k

findQuery
  :: (FromRow a) => TableName a -> Key a -> Connection -> IO (Maybe (Record a))
findQuery (TableName n) (Key k) conn =
  headMay <$> query conn "select * from ? where id = ?;" (n, k)

updateElement :: (Table a) => Key a -> a -> Connection -> IO (Record a)
updateElement key record conn = do
  let (ConstructedQuery q e) = updateQuery tableName insertValueList key record
  res <- execute conn q e
  if res == 1 then pure $ Record key record else error "bad update"

--find404 :: ( Table a
     --, FromRow a
     --, SpockConn (ActionCtxT ctx m) ~ Connection
     --, Monad m
     --, HasSpock m
     --, HasSpock (ActionCtxT ctx m)
     --, MonadIO m
     --) => Key a -> ActionCtxT ctx m (Record a)
--find404 a = do
  --rec <- Lib.find a
  --val <- guardNotFound rec
  --return val

--getAll :: (Table a, FromRow a, HasSpock m, SpockConn m ~ Connection) => m ([Record a])
--getAll = runQuery $ getFromTable tableName

getAll :: (Table a, FromRow a) => Connection -> IO [Record a]
getAll = getFromTable tableName

{-we need the second layer of function here to make the type inference find the-}
{-right "tableName function in getAll"-}
getFromTable :: (FromRow a) => TableName a -> Connection -> IO ([Record a])
getFromTable (TableName n) conn = query conn "select * from ?;" (Only n)

{-Generic insert logic-}
--insertRecord :: (Table a, HasSpock m, SpockConn m ~ Connection) => a -> m (Record a)
--insertRecord = runQuery . insertElement

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
  let paramString =
        intercalate "," $ replicate (length $ fromValueList values) "?"
  in  ConstructedQuery
        (  fromString
        $  "insert into ? ("
        ++ paramString
        ++ ") values ("
        ++ paramString
        ++ ") returning *;"
        )
        ([toField n] ++ (toRow values) ++ (toRow a))

updateQuery
  :: (Table a) => TableName a -> ValueList a -> Key a -> a -> ConstructedQuery a
updateQuery (TableName n) values k record =
  let paramString =
        intercalate "," $ replicate (length $ fromValueList values) "?"
  in  ConstructedQuery
        (  fromString
        $  "update ? set ("
        ++ paramString
        ++ ") = ("
        ++ paramString
        ++ ") where id = ?;"
        )
        ([toField n] ++ (toRow values) ++ (toRow record) ++ [toField k])


{-with added debugging-}
executeInsertQuery
  :: (ToRow a, FromRow a) => ConstructedQuery a -> Connection -> IO (Record a)
executeInsertQuery (ConstructedQuery q e) conn =
  head <$> query conn (trace (show q) q) (trace (show e) e)

{-Generic delete logic-}
--deleteRecord :: (Table a, HasSpock m, SpockConn m ~ Connection) => Key a -> m (Int64)
--deleteRecord = runQuery . deleteElement

deleteElement :: (Table a) => Key a -> Connection -> IO (Int64)
deleteElement k = executeDeleteQuery (deleteQuery tableName k)

deleteQuery :: TableName a -> Key a -> ConstructedQuery a
deleteQuery (TableName n) k =
  ConstructedQuery "delete from ? where id = ?;" [toField n, toField k]

executeDeleteQuery :: ConstructedQuery a -> Connection -> IO (Int64)
executeDeleteQuery (ConstructedQuery q vars) conn = execute conn q vars

--guardNotFound :: MonadIO m => Maybe (Record a) -> ActionCtxT ctx m (Record a)
--guardNotFound Nothing = do
  --setStatus status404
  --text "Not found"
--guardNotFound (Just a) = return a
