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
import Data.Aeson.Compat
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
{-import SuperRecordExtra-}


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
    mimeRender _ = pack

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "cards" :> ReqBody '[JSON] NewCardBody :> Post '[JSON] (Record Card)
      :<|> "cards" :> Get '[JSON] [Record Card]
      :<|> "categories" :> Get '[JSON] [Record Category]
      :<|> Get '[HTML] String
      :<|> "static" :> Raw

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic

instance ToJSON HelloMessage

{-data NewCardBody = NewCardBody-}
  {-{ question :: String-}
  {-, answer :: String-}
  {-, categoryId :: Maybe Int-}
  {-, newCategory :: String-}
  {-} deriving Generic-}
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

server3 :: Pool Connection -> Server API
server3 pool =
  position
    :<|> hello
    :<|> card
    :<|> cards
    :<|> categories
    :<|> home
    :<|> serveDirectoryWebApp "static/static"
 where
  position :: Int -> Int -> Handler Position
  position x y = return (Position x y)

  hello :: Maybe String -> Handler HelloMessage
  hello mname = return . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

  cards :: Handler [Record Card]
  cards = liftIO . withResource pool $ getAll' 

  categories :: Handler [Record Category]
  categories = liftIO . withResource pool $ getAll' 

  card :: NewCardBody -> Handler (Record Card)
  card b = liftIO . withResource pool $ \conn -> do
    cid <- case get #categoryId b of
      Just i  -> pure i
      Nothing -> key <$> insertElement (Category $ get #newCategory b) conn
    time <- getCurrentTime
    let due = addUTCTime 15 time
    insertElement (Card (get #question b) (get #answer b) time due cid) conn

  {-card2 :: NewCardBody -> ReaderT Connection IO (Record Card)-}
  {-card2 b = do-}
    {-cid <- case get #categoryId b of-}
      {-Just i  -> pure i-}
      {-Nothing -> key <$> (ReaderT $ insertElement (Category $ get #newCategory b))-}
    {-time <- liftIO getCurrentTime-}
    {-let due = addUTCTime 15 time-}
    {-ReaderT $ insertElement (Card (get #question b) (get #answer b) time due cid)-}

  {-card2 :: NewCardBody -> ReaderT Connection IO Int -}
  {-card2 b = reader (\c -> 0)-}

  home = liftIO $ do
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
