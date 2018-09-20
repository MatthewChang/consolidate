{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-} -- To test type checking
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module SubRecordTypeCheckSpec where

import Test.Hspec
import SchemaMatch
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import           SuperRecord
import           SuperRecordExtra


type Schema = '["user" := User, "money" := Int]
type User = Record '["name" := String, "age" := Int]

single = (& rnil)

perform :: (SubRecordR input Schema) => (Rec input) -> Int
perform _ = 5

int :: Int
int = 5

str :: String
str = "test"

spec :: Spec
spec = do
  describe "Recursive Sub Record Constraint" $ do
    it "should disallow mismatched names" $ do
      shouldNotTypecheck $ perform $ #nokey := int & rnil

    it "should allow matched names" $ do
      5 `shouldBe` (perform $ #money := int & rnil)

    it "should disallow mismatched types" $ do
      shouldNotTypecheck $ (perform $ #money := str & rnil)

    it "should allow nested sub records" $ do
      5 `shouldBe` (perform $ single $ #user := (single $ #name := str))
      5 `shouldBe` (perform $ single $ #user := (#name := str &! #age := int))

    it "should disallow bad nested sub records" $ do
      shouldNotTypecheck (perform $ single $ #user := (single $ #name := int))
      shouldNotTypecheck (perform $ single $ #user := (single $ #other := int))
