{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy.Char8 (ByteString,pack)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Control.Monad

data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
    mimeRender _ = pack

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> Get '[HTML] String
      :<|> "static" :> Raw

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email
emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
 where
  from'    = "great@company.com"
  to'      = clientEmail c
  subject' = "Hey " ++ clientName c ++ ", we miss you!"
  body' =
    "Hi "
      ++ clientName c
      ++ ",\n\n"
      ++ "Since you've recently turned "
      ++ show (clientAge c)
      ++ ", have you checked out our latest "
      ++ intercalate ", " (clientInterestedIn c)
      ++ " products? Give us a visit!"

server3 :: Server API
server3 = position :<|> hello :<|> marketing :<|> home :<|> serveDirectoryWebApp "static/static"
 where
  position :: Int -> Int -> Handler Position
  position x y = return (Position x y)

  hello :: Maybe String -> Handler HelloMessage
  hello mname = return . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

  marketing :: ClientInfo -> Handler Email
  marketing clientinfo = return (emailForClient clientinfo)

  home = liftIO $ do
    handle <- openFile "static/index.html" ReadMode
    hGetContents handle

userAPI :: Proxy API
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server3

main :: IO ()
main = run 8080 app1