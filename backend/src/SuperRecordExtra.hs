{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module SuperRecordExtra where

import SuperRecord
import Data.Proxy
import GHC.TypeLits

--to avoid having to have rnil in all record definitions
(&!) a b = a & b & rnil
infixr 5 &!

add l e r = l := e & r

-- Coppied from super record because it is not exported
type RecVecIdxPos l lts = RecSize lts - RecTyIdxH 0 l lts - 1

-- Constraint that you can add a label (l) with type (t) to the record (lts) resulting
-- in a record (out), which is lts with the new entry sorted in
type Addable l t lts out = (out ~ Sort (l := t ': lts), KeyDoesNotExist l lts, KnownSymbol l, KnownNat (RecSize lts), RecCopy lts lts out, KnownNat (RecVecIdxPos l out))

--remove
type family Removed l (lts :: [*]) where
  Removed l (l := v ': rest) = Removed l rest
  Removed l (a := v ': rest) = a := v ': (Removed l rest)
  Removed l '[] = '[]

type Removable l lts = Remove l lts lts (Removed l lts)

remove
  :: forall lts l
   . (Removable l lts)
  => FldProxy l
  -> Rec lts
  -> Rec (Removed l lts)
remove = remove' (Proxy :: Proxy lts)

-- Working
class Remove l (lts :: [*]) (remain :: [*]) (out :: [*]) | l remain -> out where
  remove' :: Proxy remain -> FldProxy l -> Rec lts -> Rec out

instance Remove l lts '[] '[] where
  remove' _ _ _ = rnil

instance {-# OVERLAPPABLE #-} (Has hl inp hv
  , Addable hl hv (Removed rl rest) out
  , Remove rl inp rest (Removed rl rest)) => Remove rl inp (hl := hv ': rest) out where
  remove' _ rl input = (hl := (get hl input)) & (remove' (Proxy :: Proxy rest) rl input) where
    hl = FldProxy :: FldProxy hl

instance (Remove rl lts rest out,out ~ Removed rl rest) => Remove rl lts (rl := v ': rest) out where
  remove' _ rl input = (remove' (Proxy :: Proxy rest) rl input) where

--Constraint that you can modify a record (lts) with key "l" of type "v1" to type "v2" resulting in a
--record with types "out"
type Modifyable l v1 v2 lts out = (Removable l lts, Addable l v2 (Removed l lts) out, Has l lts v1)

--Change the value of a key l, allowing types to change. Slower than modify with the same type
modifyType
  :: forall l v lts t out
   . (Modifyable l v t lts out)
  => FldProxy l
  -> (v -> t)
  -> Rec lts
  -> Rec out
modifyType l f r = l := nv & (remove l r) where nv = f $ get l r


--type aware set
data SetResult = Same | Diff | New

type family SettableSwitch (l :: Symbol) t lts :: SetResult where
  SettableSwitch l t '[] = 'New
  SettableSwitch l t (l := t ': rest) = 'Same
  SettableSwitch l t (l := o ': rest) = 'Diff
  SettableSwitch l t (a := o ': rest) = SettableSwitch l t rest

set
  :: forall s l t lts out
   . (Settable' s l t lts out, s ~ SettableSwitch l t lts)
  => FldProxy l
  -> t
  -> Rec lts
  -> Rec out
set = set' (Proxy :: Proxy s)

class Settable' (switch :: SetResult) l t (lts :: [*]) (out :: [*]) | switch l t lts -> out where
  set' :: Proxy switch -> FldProxy l -> t -> Rec lts -> Rec out

-- when list already has key with same type
instance (Has l lts t) => Settable' 'Same l t lts lts where
  set' _ l v = modify l (const v)

-- when list has key with different type
instance (Modifyable l a v lts out) => Settable' 'Diff l v lts out where
  set' _ l v = modifyType l (const v)

-- when list does not have key
instance (KeyDoesNotExist l lts, Addable l t lts out) => Settable' 'New l t lts out where
  set' _ l v = rcons (l := v)
