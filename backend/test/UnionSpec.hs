{-# OPTIONS_GHC -fdefer-type-errors #-} -- To test type checking
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables             #-}

module UnionSpec where

import Test.Hspec
import Union
import Test.ShouldNotTypecheck (shouldNotTypecheck)



testFunction :: (Num b) => [a + b] -> ([a], [b])
testFunction (x : xs) = (a', b')
 where
  (a, b)   = testFunction xs
  mi       = (get x)
  (a', b') = case mi of
    Just val -> (a, val + 1 : b)
    Nothing  -> (a, b)
testFunction [] = ([], [])

spec :: Spec
spec = do
  describe "Generic Union" $ do
    it "Creates based on arbitrary depty" $ do
      (set (5 :: Int) :: Either String Int) `shouldBe` Right 5
      (set (5 :: Int) :: Either Int String) `shouldBe` Left 5
      (set 'a' :: Either Char (Either Int String)) `shouldBe` Left 'a'
      (set "test" :: Either Char (Either Int String))
        `shouldBe` Right (Right "test")
    it "Works symetrically" $ do
      (set 'a' :: Either Char (Either Int String)) `shouldBe` Left 'a'
      (set 'a' :: Either (Either Int String) Char) `shouldBe` Right 'a'
    it "Should typecheck properly with type inference" $ do
      {-This should type check because the types match-}
      testFunction [set "test", set (5 :: Int), set "another"]
        ==         (([], [6]) :: ([String], [Int]))
        `shouldBe` True
      {-this should not type check because string should fail tye typeclass for num-}
      {-correctly doesn't typecheck but I'm cant put it in the tests-}
      shouldNotTypecheck
        $  testFunction [set "test", set (5 :: Int), set "another"]
        == (([], [6]) :: ([Int], [String]))
      {-this should not type check because the inputs do not match the outputs-}
      shouldNotTypecheck
        $  testFunction [set "test", set 'a']
        == (([], []) :: ([Int], [String]))

    {-it "Should typecheck with typeclasses" $ do-}
      {-1 `shouldBe` 1-}
    it "allows data retrieval" $ do
      get (Right 5 :: Either String Int) `shouldBe` Just (5 :: Int)
      get (Left "test" :: Either String Int)
        `shouldBe` (Just "test" :: Maybe String)
      get (Left "test" :: Either String Int) `shouldBe` (Nothing :: Maybe Int)
      get (Right (Left 10) :: Either Char (Either Int String))
        `shouldBe` Just (10 :: Int)
    it "works with type operators" $ do
      get (set (5 :: Int) :: String + Int) `shouldBe` Just (5 :: Int)
      get (set "test" :: String + Int + Char) `shouldBe` Just "test"
      get (set "test" :: Char + Float + String) `shouldBe` Just "test"
    it "should not allow assignment for unions not containing the type" $ do
      shouldNotTypecheck (set 5 :: Either Char (Either String Float))
      shouldNotTypecheck (set 5 :: Either Char String)
    it "should allow redundant unions to typecheck" $ do
      set (5 :: Int) `shouldBe` (Right 5 :: Either Int Int)

    it "should not allow typecheck retrieval for unions not containing the type"
      $ do
          shouldNotTypecheck (get (Left "string" :: Either Int Char))

    it "should work with non union types" $ do
      get (set (5 :: Int) :: Int) `shouldBe` Just (5 :: Int)
      (set (5 :: Int) :: Int) `shouldBe` (5 :: Int)
