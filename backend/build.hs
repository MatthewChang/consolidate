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

main = shelly $ verbosely $ do
  runRaw "docker build . -t flashcards"
  path <- runRaw "docker run flashcards stack path --local-install-root"
  id   <- runRaw "docker create flashcards"
  runRaw $ "docker cp " <> id <> ":" <> path <> "/bin/flashcards ./exe"
  runRaw $ "docker rm -v " <> id
  runRaw "docker build . -t deploy -f DeployDockerfile"
