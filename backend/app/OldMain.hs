{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators             #-}

module OldMain where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Network.Wai.Middleware.Static
import           Web.Spock                            hiding (head)
import           Web.Spock.Config
import           Data.Aeson                           hiding (json)
import           Lib
import           Tables
import           GHC.Generics
import           Data.Text (Text)

type Api = SpockM Connection () () ()
type ApiAction a = SpockAction Connection () () a

errorJson :: Int -> String -> ApiAction ()
errorJson code message = json $ object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

split3 :: [(a :. b :. c)] -> ([a], [b], [c])
split3 []               = ([], [], [])
split3 ((a :. b :. c) : xs) = (a : as, b : bs, c : cs)
  where (as, bs, cs) = split3 xs

oldMain :: IO ()
oldMain = do
  pool <- createPool (connect (ConnectInfo "localhost" 5432 "" "" "music"))
                     close
                     1
                     10
                     10
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockCfg app)

data CardInput = CardInput {
    question :: Text,
    answer :: Text,
    categoryId :: Key Category
} deriving (Show, Generic)


app :: Api
app = do
  middleware (staticPolicy (addBase "static"))
  get root $ do
    file "" "static/index.html"

  {-get "tags" $ do-}
    {-tags <- getAll-}
    {-json (tags :: [Record Tag])-}

  {-get "songs" $ do-}
    {-songs <- getAll-}
    {-json (songs :: [Record Song])-}

  {-post "cards" $ do-}
    {-card <- jsonBody :: ApiAction (Maybe Tag)-}
    {-case maybeTag of-}
      {-Nothing  -> errorJson 422 "Failed to parse request body as Tag"-}
      {-Just tag -> do-}
        {-tagr <- insertRecord tag-}
        {-json tagr-}

  {-post "songs" $ do-}
    {-maybeSong <- jsonBody :: ApiAction (Maybe Song)-}
    {-case maybeSong of-}
      {-Nothing   -> errorJson 422 "Failed to parse request body as Song"-}
      {-Just song -> do-}
        {-sr <- insertRecord song-}
        {-json sr-}

  {-post "songTags" $ do-}
    {-maybeSong <- jsonBody :: ApiAction (Maybe SongTag)-}
    {-case maybeSong of-}
      {-Nothing -> errorJson 422 "Failed to parse request body as SongTag"-}
      {-Just st -> do-}
        {-record <- insertRecord st-}
        {-json record-}

  {-Web.Spock.delete ("songs" <//> var) $ \songId -> do-}
    {-deleted <- deleteRecord $ (Key songId :: Key Song)-}
    {-json deleted-}

  {-get ("home") $ do-}
    {-songs    <- getAll-}
    {-tags     <- getAll-}
    {-songTags <- getAll-}
    {-json $ object-}
      {-[ "tags" .= (tags :: [Record Tag])-}
      {-, "songs" .= (songs :: [Record Song])-}
      {-, "songTags" .= (songTags :: [Record SongTag])-}
      {-]-}

  {-get ("tags" <//> var) $ \tagId -> do-}
    {-tag    <- find404 $ (Key tagId :: Key Tag)-}
    {-result <- executeQuery $ build $ And (SongTagsTagId =. tagId)-}
                                         {-(SongsId =. SongTagsSongId)-}
    {-(songs, songTags, tags) <- pure-}
      {-$ split3 (result :: [(Record Song) :. (Record SongTag) :. (Record Tag)])-}

    {-json $ object-}
      {-[ "tags" .= (tags :: [Record Tag])-}
      {-, "songTags" .= (songTags :: [Record SongTag])-}
      {-, "songs" .= (songs :: [Record Song])-}
      {-]-}

