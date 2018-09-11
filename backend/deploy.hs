#!/usr/bin/env stack
-- stack --resolver lts-12.6 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import Data.Text as T
default (T.Text)

runRaw :: Text -> Sh Text
runRaw t = T.strip <$> run (fromText first) rest
  where (first : rest) = T.splitOn " " t

app = "mchang-flashcards"

main = shelly $ verbosely $ do
  runRaw "./build.hs"
  runRaw $ "heroku container:push web --app " <> app
  runRaw $ "heroku container:release web --app " <> app
