{-# OPTIONS_GHC -fdefer-type-errors #-} -- To test type checking
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module LibTypeCheckSpec where

import Test.Hspec
import Lib
import TestTables
import           Database.PostgreSQL.Simple.Types
import Test.ShouldNotTypecheck (shouldNotTypecheck)

spec :: Spec
spec = do
  describe "Insert Query" $ do
    it "checks constraints against column types" $ do
      shouldNotTypecheck
        $ show
        $ (build (songTagTagIdC =. songIdC) :: ConstructedQuery
              (Record Song :. Record SongTag :. Record Tag)
          )

      shouldNotTypecheck
        $ show
        $ (build (songTagSongIdC =. (5 :: Int)) :: ConstructedQuery
              (Record Song :. Record SongTag :. Record Tag)
          )
