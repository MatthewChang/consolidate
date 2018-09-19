{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module SuperRecordExtra where

import SuperRecord

--to avoid having to have rnil in all record definitions
(&!) a b = a & b & rnil
infixr 5 &!

