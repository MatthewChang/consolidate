{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE TypeOperators        #-}

module Union where

data HTrue
{-represent tree node with the right type on the left or right-}
data HLeafLeft
data HLeafRight
data HFalse
{-represent tree node with the right type in a descendant on the left or right-}
data HLeft
data HRight

type a + b = Either a b

{-boolean or at type level-}
type family Or a b where
  Or HTrue HTrue = HLeft
  Or HTrue HFalse = HLeft
  Or HFalse HTrue = HRight
  Or HFalse HFalse = HFalse

{-convert types to boolean types-}
type family ToBool a where
  ToBool HTrue = HTrue
  ToBool HLeafLeft = HTrue
  ToBool HLeafRight = HTrue
  ToBool HLeft = HTrue
  ToBool HRight = HTrue
  ToBool a = HFalse

{-type function to indicate how recursion should happen-}
{-HTrue indicates that we the type i can be found directly in u-}
{-HLeft indicates we can find the type i in the left branch of u-}
{-HRight indicates we can find the type i in the right branch of u-}
{-HFalse indicates the type i cannot be found in u-}
type family SetSwitch i u where
  {-using the same output variable for (Either a i) and (Either i a) -}
  {-causes usage with (Either i i) to fail to typecheck-}
  {-this is intentional for now-}
  SetSwitch i (Either a i) = HLeafRight
  SetSwitch i (Either i a) = HLeafLeft
  SetSwitch i (Either a b) = Or (ToBool (SetSwitch i a)) (ToBool (SetSwitch i b))
  SetSwitch i a = HFalse

class Setable i u where
  set :: i -> u
  get :: u -> Maybe i

class Setable' flag i u where
  set' :: flag -> i -> u
  get' :: flag -> u -> Maybe i

instance (SetSwitch i u ~ flag, Setable' flag i u) => Setable i u where
  set = set' (undefined :: flag)
  get = get' (undefined :: flag)

instance Setable' HLeafLeft i (Either i a) where
  set' _ = Left
  get' _ (Left i) = Just i
  get' _ (Right _) = Nothing

instance Setable' HLeafRight i (Either a i) where
  set' _ = Right
  get' _ (Left _) = Nothing
  get' _ (Right i) = Just i

instance Setable i a => Setable' HLeft i (Either a b) where
  set' _ = Left . set
  get' _ (Right _) = Nothing
  get' _ (Left x) = get x

instance Setable i b => Setable' HRight i (Either a b) where
  set' _ = Right . set
  get' _ (Left _) = Nothing
  get' _ (Right x) = get x
