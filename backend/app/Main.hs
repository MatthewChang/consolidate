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

module ApiType where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Data.Aeson.Compat hiding (value)
import Data.ByteString.Lazy.Char8 (pack)
import GHC.Generics
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
import Control.Monad.Trans.Reader
import SuperRecordExtra


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
    mimeRender _ = pack


{-note the keys have to be provided in alphabetical order for this to work for some reason-}
type ShowAllResponse = Rec '["cards" := [Record Card], "categories" := [Record Category]]
type GetCardResponse = Rec '["card" := Record Card, "categories" := [Record Category]]

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "cards" :> ReqBody '[JSON] NewCardBody :> Post '[JSON] (Record Card)
      :<|> "cards" :> Get '[JSON] [Record Card]
      :<|> "cards" :> Capture "id" (Key Card) :> Get '[JSON] GetCardResponse
      :<|> "cards" :> Capture "id" (Key Card) :> "correct" :> Post '[JSON] (Record Card)
      :<|> "cards" :> Capture "id" (Key Card) :> Delete '[JSON] (Key Card)
      :<|> "cards" :> Capture "id" (Key Card) :> ReqBody '[JSON] NewCardBody :> Put '[JSON] (Record Card)
      :<|> "categories" :> Get '[JSON] [Record Category]
      :<|> "all" :> Get '[JSON] ShowAllResponse
      :<|> Get '[HTML] String
      :<|> "static" :> Raw

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic

instance ToJSON HelloMessage

type NewCardBody = Rec '["question" := Text
                  , "answer" := Text
                  , "categoryId" := Maybe (Key Category)
                  , "newCategory" := Text]

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

connectionPool :: IO (Pool Connection)
connectionPool = createPool
  (connect (ConnectInfo "localhost" 5432 "" "" "flashcards"))
  close
  1
  10
  10

cardsCorrect :: Key Card -> Handler (Record Card)
cardsCorrect = undefined

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
  position
    :<|> hello
    :<|> card
    :<|> cards
    :<|> showCard
    :<|> cardsCorrect
    :<|> cardDelete
    :<|> edit
    :<|> categories
    :<|> showAll
    :<|> home
    :<|> serveDirectoryWebApp "static/static"
 where
  position :: Int -> Int -> Handler Position
  position x y = return (Position x y)

  edit :: Key Card -> NewCardBody -> Handler (Record Card)
  edit k b = withResource pool $ \conn -> cardEdit k b conn

  hello :: Maybe String -> Handler HelloMessage
  hello mname = return . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

  cards :: Handler [Record Card]
  cards = liftIO . withResource pool $ getAll

  showCard :: Key Card -> Handler GetCardResponse
  showCard k = withResource pool $ \conn -> do 
    c <- find404 k conn
    cats <- liftIO $ getAll conn
    pure $ #card := c &! #categories := cats

  categories :: Handler [Record Category]
  categories = liftIO . withResource pool $ getAll

  showAll :: Handler ShowAllResponse
  showAll = liftIO . withResource pool $ \conn -> do
    let f a b = #cards := a &! #categories := b
    liftM2 f (getAll conn) (getAll conn)

  card :: NewCardBody -> Handler (Record Card)
  card b = liftIO . withResource pool $ \conn -> do
    cid <- case get #categoryId b of
      Just i  -> pure i
      Nothing -> key <$> insertElement (Category $ get #newCategory b) conn
    time <- getCurrentTime
    let due = addUTCTime 15 time
    insertElement (Card (get #question b) (get #answer b) time due cid) conn

  cardDelete :: Key Card -> Handler (Key Card)
  cardDelete = fmap Key . liftIO . withResource pool . deleteElement

  home       = liftIO $ do
    handle <- openFile "static/index.html" ReadMode
    hGetContents handle

userAPI :: Proxy API
userAPI = Proxy

nt :: Pool Connection -> ReaderT Connection IO x -> Handler x
nt pool (ReaderT r) = liftIO $ withResource pool r

{-modifiedServer :: Pool Connection -> Server API-}
{-modifiedServer pool = hoistServer userAPI (nt pool) $ server3 pool-}
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Pool Connection -> Application
app1 pool = serve userAPI $ server3 pool

main :: IO ()
main = do
  pool <- connectionPool
  run 8080 $ logStdoutDev $ app1 pool
