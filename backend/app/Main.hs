{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (pack)
--import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import           Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Tables
import Lib
import Data.Time.Clock
import SuperRecord hiding (Record)
import           Data.Pool
import           Database.PostgreSQL.Simple
import Network.Wai.Middleware.RequestLogger
import SuperRecordExtra
import Safe
import System.Environment
import Control.Exception (tryJust)
import System.IO.Error
import qualified Data.ByteString.Char8 as B


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
    mimeRender _ = pack

--note the keys have to be provided in alphabetical order for this to work for some reason
type ShowAllResponse = Rec '["cards" := [Record Card], "categories" := [Record Category]]
type GetCardResponse = Rec '["card" := Record Card, "categories" := [Record Category]]
type ReadyCardResponse = Rec '["card" := Maybe (Record Card), "categories" := [Record Category]]
type NewCardBody = Rec '["question" := Text
                  , "answer" := Text
                  , "categoryId" := Maybe (Key Category)
                  , "newCategory" := Text]

type API =  "cards" :> ReqBody '[JSON] NewCardBody :> Post '[JSON] [Record Category]
      :<|> "cards" :> Get '[JSON] [Record Card]
      :<|> "cards" :> "ready" :> Get '[JSON] ReadyCardResponse
      :<|> "cards" :> Capture "id" (Key Card) :> Get '[JSON] GetCardResponse
      :<|> "cards" :> Capture "id" (Key Card) :> "correct" :> Post '[JSON] ReadyCardResponse
      :<|> "cards" :> Capture "id" (Key Card) :> "wrong" :> Post '[JSON] ReadyCardResponse
      :<|> "cards" :> Capture "id" (Key Card) :> Delete '[JSON] (Key Card)
      :<|> "cards" :> Capture "id" (Key Card) :> ReqBody '[JSON] NewCardBody :> Put '[JSON] (Record Card)
      :<|> "categories" :> Get '[JSON] [Record Category]
      :<|> "all" :> Get '[JSON] ShowAllResponse
      :<|> Get '[HTML] String
      :<|> "static" :> Raw

getConnectionString :: IO B.ByteString
getConnectionString = do
  r <- tryJust (guard . isDoesNotExistError) $ getEnv "DATABASE_URL"
  return $ case r of
    Left _ -> postgreSQLConnectionString $ ConnectInfo "localhost" 5432 "" "" "flashcards"
    Right home -> B.pack home

connectionPool :: IO (Pool Connection)
connectionPool = do
  dbString <- getConnectionString
  putStrLn $ "Connecting to: " ++ B.unpack dbString
  createPool (connectPostgreSQL dbString) close 1 10 10


cardEdit :: Key Card -> NewCardBody -> Connection -> Handler (Record Card)
cardEdit k b conn = do
  card <- value <$> find404 k conn
  let q = get #question b
      a = get #answer b
  cid <- liftIO $ newOrExistingCategoryId b conn
  let updatedCard = card { question = q, answer = a, categoryId = cid }
  liftIO $ updateElement k updatedCard conn

find404 :: (Table a, FromRow a) => Key a -> Connection -> Handler (Record a)
find404 k conn = do
  mRec <- liftIO $ findConnection k conn
  case mRec of
    Just i  -> pure i
    Nothing -> throwError err404

newOrExistingCategoryId :: NewCardBody -> Connection -> IO (Key Category)
newOrExistingCategoryId b conn = case get #categoryId b of
  Just i  -> pure i
  Nothing -> key <$> insertElement (Category $ get #newCategory b) conn

server3 :: Pool Connection -> Server API
server3 pool =
  card
    :<|> cards
    :<|> ready
    :<|> showCard
    :<|> cardsCorrectWrong True
    :<|> cardsCorrectWrong False
    :<|> cardDelete
    :<|> edit
    :<|> categories
    :<|> showAll
    :<|> home
    :<|> serveDirectoryWebApp "static/static"
 where
  edit :: Key Card -> NewCardBody -> Handler (Record Card)
  edit k b = withResource pool $ \conn -> cardEdit k b conn

  ready :: Handler ReadyCardResponse
  ready = liftIO . withResource pool $ \conn -> do
    time <- getCurrentTime
    cs   <- (runQuery . build $ time >=. cardDueAtC) conn
    cats <- getAll conn
    return $ #card := (headMay cs) &! #categories := cats

  cards :: Handler [Record Card]
  cards = liftIO . withResource pool $ getAll

  showCard :: Key Card -> Handler GetCardResponse
  showCard k = withResource pool $ \conn -> do
    c    <- find404 k conn
    cats <- liftIO $ getAll conn
    pure $ #card := c &! #categories := cats

  cardsCorrectWrong :: Bool -> Key Card -> Handler ReadyCardResponse
  cardsCorrectWrong res k = withResource pool $ \conn -> do
    c    <- find404 k conn
    time <- liftIO getCurrentTime
    let inter = if res
          then 2 * (diffUTCTime time $ lastAnsweredAt $ value c)
          else (diffUTCTime (dueAt $ value c) (lastAnsweredAt $ value c)) / 2
        nextTime    = addUTCTime inter time
        updatedCard = (value c) { lastAnsweredAt = time, dueAt = nextTime }
    _ <- liftIO $ updateElement k updatedCard conn
    ready

  categories :: Handler [Record Category]
  categories = liftIO . withResource pool $ getAll

  showAll :: Handler ShowAllResponse
  showAll = liftIO . withResource pool $ \conn -> do
    let f a b = #cards := a &! #categories := b
    liftM2 f (getAll conn) (getAll conn)

  card :: NewCardBody -> Handler [Record Category]
  card b = liftIO . withResource pool $ \conn -> do
    cid <- case get #categoryId b of
      Just i  -> pure i
      Nothing -> key <$> insertElement (Category $ get #newCategory b) conn
    time <- getCurrentTime
    let due = addUTCTime 15 time
    _ <- insertElement (Card (get #question b) (get #answer b) time due cid)
                       conn
    getAll conn

  cardDelete :: Key Card -> Handler (Key Card)
  cardDelete = fmap Key . liftIO . withResource pool . deleteElement

  home       = liftIO $ do
    handle <- openFile "static/index.html" ReadMode
    hGetContents handle

userAPI :: Proxy API
userAPI = Proxy

--nt :: Pool Connection -> ReaderT Connection IO x -> Handler x
--nt pool (ReaderT r) = liftIO $ withResource pool r

--modifiedServer :: Pool Connection -> Server API
--modifiedServer pool = hoistServer userAPI (nt pool) $ server3 pool
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Pool Connection -> Application
app1 pool = serve userAPI $ server3 pool

main :: IO ()
main = do
  pool <- connectionPool
  run 8080 $ logStdoutDev $ app1 pool
