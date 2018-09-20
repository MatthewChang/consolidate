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

--type Schema = '["user" := User, "money" := Int]
--type User = Rec '["name" := String, "age" := Int]

--type GoodTestRecord = '["user" := User]
--type EasyBadRecord = '["user" := Int]
--type HardGoodRecord = '["user" := Rec '["name" := String]]
--type HardBadRecord = '["user" := Rec '["name" := Int]]



--test :: Has "name" '["name" := String] Int => Int
--test = 5

--testBad :: HasOf '["name" := String] '["name" := Int] => Int
--testBad = 5

-- Returns the type from a record property if it exists, error otherwise
type family RecTy (l :: Symbol) (lts :: [*]) :: * where
    RecTy l (l := t ': lts) = t
    RecTy q (l := t ': lts) = RecTy q lts

-- Returns the list of properties from a record, error otherwise
type family UnRec (r :: * ) :: [*] where
    UnRec (Rec a) = a

-- Returns a constraint that passes when the list of properties tls has symbol l with type t
-- if t is also a Record, we check recirsivly that t is a subrecord of lts[l] (the element of lts that has label l) 
-- naturally, the constraint will fail if t is a Record but lts[l] is not
type family HasR (l :: Symbol) (lts :: [*]) (t :: *) :: Constraint where
  HasR l lts (Rec a) = (SubRecordR a (UnRec (RecTy l lts)))
  HasR l lts t = Has l lts t

-- Returns a constraint that passes when the list of properties req represents a recursive sub record of tls
type family SubRecordR (req :: [*]) (lts :: [*]) :: Constraint where
    SubRecordR (l := t ': req) lts = (HasR l lts t, SubRecordR req lts)
    SubRecordR '[] lts = 'True ~ 'True
