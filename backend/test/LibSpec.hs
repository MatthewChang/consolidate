{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module LibSpec where

import Test.Hspec
import Lib
import TestTables
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.ToField


spec :: Spec
spec = do
  describe "Insert Query" $ do
    it "builds a reasonable query" $ do
      let expected =
            ConstructedQuery "insert into ? (?) values (?) returning *;"
              $   toField
              <$> [ toField $ Identifier "songs"
                  , toField (Identifier "name")
                  , toField ("test" :: String)
                  ]
      (insertQuery tableName insertValueList $ Song "test")
        `shouldBe` (expected)


    {-it "builds a reasonable query" $ do-}
      {-let expected =-}
            {-ConstructedQuery-}
              {-. fromString-}
              {-$ "insert into songtags(songid,tagid) values (?,?) returning *;"-}
      {-(insertQuery tableName insertValueList)-}
        {-`shouldBe` (expected :: ConstructedQuery SongTag)-}
  describe "Update query" $ do
    it "builds a reasonable query" $ do
      let expected =
            ConstructedQuery "update ? set (?) = (?) where id = ?;"
              $ [ toField $ Identifier "songs"
                , toField ("name" :: String)
                , toField ("test" :: String)
                , toField (1 :: Int)
                ]
      (updateQuery tableName insertValueList (Key 1) $ Song "test")
        `shouldBe` (expected)


  describe "insertValueList" $ do
    it "does not return the primary key by default" $ do
      let expected = insertValueList :: ValueList Song
      ValueList ["name"] `shouldBe` expected

  describe "toQuery" $ do
    it "works with automatic lifting for unions" $ do
      let result =
            buildConstraintQuery ((5 :: Int) =. (8 :: Int)) :: ConstructedQuery
                (Record Song :. Record SongTag :. Record Tag)
      constructedQuery "? = ?" (5 :: Int, 8 :: Int) `shouldBe` result

      let result2 =
            buildConstraintQuery ((5 :: Int) <. (8 :: Int)) :: ConstructedQuery
                (Record Song :. Record SongTag :. Record Tag)
      constructedQuery "? < ?" (5 :: Int, 8 :: Int) `shouldBe` result2

      let result3 =
            buildConstraintQuery ((8 :: Int) >=. (8 :: Int)) :: ConstructedQuery
                (Record Song :. Record SongTag :. Record Tag)
      constructedQuery "? >= ?" (8 :: Int, 8 :: Int) `shouldBe` result3

    it "works with columns" $ do
      let
        result =
          buildConstraintQuery (songIdC =. (Key 8 :: Key Song)) :: ConstructedQuery
              (Record Song :. Record SongTag :. Record Tag)

      constructedQuery
          "? = ?"
          ( Many [EscapeIdentifier "songs", Plain ".", EscapeIdentifier "id"]
          , 8 :: Int
          )
        `shouldBe` result

    it "works with keys" $ do
      let
        result =
          buildConstraintQuery (songIdC =. (Key 10 :: Key Song)) :: ConstructedQuery
              (Record Song :. Record SongTag :. Record Tag)

      constructedQuery
          "? = ?"
          ( Many [EscapeIdentifier "songs", Plain ".", EscapeIdentifier "id"]
          , 10 :: Int
          )
        `shouldBe` result

    it "builds full query" $ do
      let
        result =
          build
            (And (songIdC =. (Key 8 :: Key Song)) (songTagSongIdC =. songIdC)) :: ConstructedQuery
              (Record Song :. Record SongTag :. Record Tag)
      result
        `shouldBe` (ConstructedQuery
                     "select * from ?, ?, ? where ? = ? and ? = ?"
                     [ EscapeIdentifier "songs"
                     , EscapeIdentifier "songtags"
                     , EscapeIdentifier "tags"
                     , Many
                       [ EscapeIdentifier "songs"
                       , Plain "."
                       , EscapeIdentifier "id"
                       ]
                     , Plain "8"
                     , Many
                       [ EscapeIdentifier "songtags"
                       , Plain "."
                       , EscapeIdentifier "songid"
                       ]
                     , Many
                       [ EscapeIdentifier "songs"
                       , Plain "."
                       , EscapeIdentifier "id"
                       ]
                     ]
                   )
