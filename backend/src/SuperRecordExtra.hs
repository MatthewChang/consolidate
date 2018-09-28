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
import GHC.Exts (Constraint)
import Data.Proxy
import GHC.TypeLits

--to avoid having to have rnil in all record definitions
(&!) a b = a & b & rnil
infixr 5 &!

--modifyType :: forall l v lts. Has l lts v => FldProxy l -> (v -> t) -> Rec lts -> Rec lts2
----modifyType = 

--type family Pop label lts where
  --Pop label (label := t ': rest) = rest
  --Pop label (other := t ': rest) = Pop label rest
  --Pop label '[] = '[]

-- Coppied from super record because it is not exported
type RecVecIdxPos l lts = RecSize lts - RecTyIdxH 0 l lts - 1

type Addable l t lts sortedLts = (sortedLts ~ Sort (l := t ': lts), KeyDoesNotExist l lts, KnownSymbol l, KnownNat (RecSize lts), RecCopy lts lts sortedLts, KnownNat (RecVecIdxPos l sortedLts))

--pop
class Pop (a :: [*]) where
  pop :: Proxy a -> String

instance (Pop rest,KnownSymbol l) => Pop ((l := v) ': rest) where
  pop _ = symbolVal (FldProxy :: FldProxy l ) ++ pop (Proxy :: Proxy rest)

instance Pop '[] where
  pop _ = "test"

copy :: forall lts . (Copy' lts lts) => Rec lts -> Rec lts
copy = copy' (Proxy :: Proxy lts)

--copy
class Copy' (lts :: [*]) (remain :: [*]) where
  copy' :: Proxy remain -> Rec lts -> Rec remain

instance Copy' lts '[] where
  copy' _ _ = rnil

instance (Has l lts v,Addable l v rest (l := v ': rest), Copy' lts rest) => Copy' lts (l := v ': rest) where
  copy' _ r = (label := (get label r)) & (copy' (Proxy :: Proxy rest) r) where
    label = FldProxy :: FldProxy l

--remove
type family Removed l (lts :: [*]) where
  Removed l (l := v ': rest) = Removed l rest
  Removed l (a := v ': rest) = a := v ': (Removed l rest)
  Removed l '[] = '[]

--old
--remove :: forall lts l . (Remove l lts lts) => FldProxy l -> Rec lts -> Rec (Removed l lts)
--remove = remove' (Proxy :: Proxy lts)

--class Remove l (lts :: [*]) (remain :: [*]) where
  --remove' :: Proxy remain -> FldProxy l -> Rec lts -> Rec (Removed l remain)

--instance Remove l lts '[] where
  --remove' _ _ _ = rnil

--instance (Has hl inp hv
  --, Addable hl hv (Removed rl rest) (hl := hv ': (Removed rl rest))
  --, Remove rl inp rest
  ----, Test rl hl hv rest
  ----, Test2 rl hl hv rest
  ---- verify that this key is not removed
  ----, (hl := hv ': (Removed rl rest)) ~ (hl := hv ': (Removed l rest))
         --) => Remove rl inp (hl := hv ': rest) where
  --remove' _ rl input = nonMatchItem & (remove' (Proxy :: Proxy rest) rl input) where
    --nonMatchItem = (hl := (get hl input))
    --hl = FldProxy :: FldProxy hl

--instance (Remove rl lts rest) => Remove rl lts (rl := v ': rest) where
  --remove' _ rl input = (remove' (Proxy :: Proxy rest) rl input) where

type family RemoveSwitch l (lts :: [*]) where
  RemoveSwitch l (l := v ': rest) = 'True
  RemoveSwitch a (l := v ': rest) = 'False

remove :: forall lts l . (Remove l lts lts (Removed l lts)) => FldProxy l -> Rec lts -> Rec (Removed l lts)
remove = remove' (Proxy :: Proxy lts)

--class Remove switch l (lts :: [*]) (remain :: [*]) (out :: [*]) | l remain -> out where
  --remove' :: Proxy switch -> Proxy remain -> FldProxy l -> Rec lts -> Rec out

--instance Remove a l lts '[] '[] where
  --remove' _ _ _ _ = rnil

--instance (Has hl inp hv
  --, Addable hl hv (Removed rl rest) out
  --, Remove rl inp rest (Removed rl rest)) => Remove 'False rl inp (hl := hv ': rest) out where
  --remove' _ _ rl input = nonMatchItem & (remove' (Proxy :: Proxy rest) rl input) where
    --nonMatchItem = (hl := (get hl input))
    --hl = FldProxy :: FldProxy hl

--instance (Remove rl lts rest out, out ~ Removed rl rest) => Remove 'True rl lts (rl := v ': rest) out where
  --remove' _ _ rl input = (remove' (Proxy :: Proxy rest) rl input) where

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
