{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}


module SchemaMatch where

import GHC.Exts (Constraint)
import           SuperRecord
import GHC.TypeLits


type Store = Rec '["name" := String, "money" := Int]

type Schema = '["user" := User, "money" := Int]
type User = Rec '["name" := String, "age" := Int]

type GoodTestRecord = '["user" := User]
type EasyBadRecord = '["user" := Int]
type HardGoodRecord = '["user" := Rec '["name" := String]]
type HardBadRecord = '["user" := Rec '["name" := Int]]



test :: Has "name" '["name" := String] Int => Int
test = 5

testBad :: HasOf '["name" := String] '["name" := Int] => Int
testBad = 5

-- Returns the type from a record property if it exists, error otherwise
type family RecTy (l :: Symbol) (lts :: [*]) :: * where
    RecTy l (l := t ': lts) = t
    RecTy q (l := t ': lts) = RecTy q lts

-- Returns the list of properties from a record, error otherwise
type family UnRec (r :: * ) :: [*] where
    UnRec (Rec a) = a

type family HasR (l :: Symbol) (lts :: [*]) (t :: *) :: Constraint where
  HasR l lts (Rec a) = (SubRecordR a (UnRec (RecTy l lts)))
  HasR l lts t = Has l lts t

--type family SubRecTy (l :: Symbol) (lts :: [*]) :: [*] where
    --SubRecTy l (l := (Rec t) ': lts) = t
    --SubRecTy q (l := t ': lts) = SubRecTy q lts

--
--type family HasR (l :: Symbol) (lts :: [*]) (t :: *) :: Constraint where
  --HasR l lts (Rec a) = (SubRecordR a (SubRecTy l lts))
  --HasR l lts t = Has l lts t

type family SubRecordR (req :: [*]) (lts :: [*]) :: Constraint where
    SubRecordR (l := t ': req) lts = (HasR l lts t, SubRecordR req lts)
    SubRecordR '[] lts = 'True ~ 'True

perform :: (SubRecordR HardGoodRecord Schema) => Int
perform = 5

