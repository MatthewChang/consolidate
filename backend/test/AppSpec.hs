{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators             #-}

module AppSpec where

import Test.Hspec
import Lib
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

  describe "insertValueList" $ do
    it "does not return the primary key by default" $ do
      let expected = insertValueList :: ValueList Song
      ValueList ["name"] `shouldBe` expected

  describe "toQuery" $ do
    it "builds a constructed query from constraints" $ do
      let expected = toQuery $ Equal (7 :: Int) (8 :: Int)
      constructedQuery "? = ?" [7 :: Int, 8 :: Int] `shouldBe` expected
      let expected2 = toQuery $ Less (7 :: Int) (8 :: Int)
      constructedQuery "? < ?" [7 :: Int, 8 :: Int] `shouldBe` expected2
      let expected3 = toQuery $ GreaterOrEqual (7 :: Int) (8 :: Int)
      constructedQuery "? >= ?" [7 :: Int, 8 :: Int] `shouldBe` expected3

    it "builds queries from combined expressions" $ do
      let expected = toQuery $ And
            (Equal ("test" :: String) ("test" :: String))
            (Equal ("other" :: String) ("other2" :: String))
      constructedQuery "? = ? and ? = ?"
                       (["test", "test", "other", "other2"] :: [String])
        `shouldBe` expected

    it "works with automatic lifting for unions" $ do
      let result =
            buildConstraintQuery ((5 :: Int) =. (8 :: Int)) :: ConstructedQuery
                (Record Song :. Record SongTag :. Record Tag)
      constructedQuery "? = ?" (5 :: Int, 8 :: Int) `shouldBe` result

    it "works with columns" $ do
      let result =
            buildConstraintQuery (SongsId =. (8 :: Int)) :: ConstructedQuery
                (Record Song :. Record SongTag :. Record Tag)

      constructedQuery
          "? = ?"
          ( Many [EscapeIdentifier "songs", Plain ".", EscapeIdentifier "id"]
          , 8 :: Int
          )
        `shouldBe` result

    it "works with keys" $ do
      let result =
            buildConstraintQuery (SongsId =. (Key 10 :: Key Song)) :: ConstructedQuery
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
          build (And (SongsId =. (8 :: Int)) (SongTagsSongId =. SongsId)) :: ConstructedQuery
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
