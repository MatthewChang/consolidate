{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE TypeOperators        #-}


module UnionDummy where

data HOne
data HTwo
data HThree

{-Dummy int instance-}
type family SetSwitchInt a where
  SetSwitchInt (Either Int a) = HOne
  SetSwitchInt (Either a Int) = HTwo
  SetSwitchInt (Either a b) = HThree
class SetableInt a where
  setInt :: Int -> a
  getInt :: a -> Maybe Int

class SetableInt' flag a where
  setInt' :: flag -> Int -> a
  getInt' :: flag -> a -> Maybe Int

instance (SetSwitchInt a ~ flag, SetableInt' flag a) => SetableInt a where
  setInt = setInt' (undefined :: flag)
  getInt = getInt' (undefined :: flag)

instance SetableInt' HOne (Either Int a) where
  setInt' _ = Left
  getInt' _ (Left i) = Just i
  getInt' _ (Right _) = Nothing

instance SetableInt' HTwo (Either a Int) where
  setInt' _ = Right
  getInt' _ (Left _) = Nothing
  getInt' _ (Right i) = Just i

instance SetableInt b => SetableInt' HThree (Either a b) where
  setInt' _ = Right . setInt
  getInt' _ (Left _) = Nothing
  getInt' _ (Right x) = getInt x
