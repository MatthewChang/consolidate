{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             TypeFamilies,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances #-}

module Main where

import Prelude hiding (print)

class Print a where
    print :: a -> IO ()

class Print' flag a where
    print' :: flag -> a -> IO ()

instance (ShowPred a ~ flag, Print' flag a) => Print a where
    print = print' (undefined::flag)


type family ShowPred a where
  ShowPred Int    = HTrue
  ShowPred Bool   = HTrue
  ShowPred [a]   = ShowPred a
  ShowPred a     = HFalse

data HTrue    -- Just two
data HFalse   -- distinct types

{-instance Show a => Print' HTrue a where-}
   {-print' _ x = putStrLn (show x)-}
{-instance Print' HFalse a where-}
   {-print' _ x = putStrLn "No show method"-}

instance Show a => Print' HTrue a where
   print' _ x = putStrLn (show x)
instance Print' HFalse a where
   print' _ x = putStrLn "No show method"

main :: IO ()
main = do
  test1 <- print [True, False] -- [True,False]
  test2 <- print id           -- No show method
  return ()




-- see http://okmij.org/ftp/Haskell/typecast.html
class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x
