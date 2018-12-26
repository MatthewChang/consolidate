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

module Auth where

import Tables
import Lib
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Trans (liftIO)
import Web.Users.Types
import Web.Users.Postgresql ()
import           Database.PostgreSQL.Simple
import Data.Text
import Data.Maybe
import           Data.Pool

{-data User = User { name :: String, email :: String }-}
   {-deriving (Eq, Show, Read, Generic)-}

{-instance ToJSON User-}
instance ToJWT User
{-instance FromJSON User-}
instance FromJWT User

data Login = Login { username :: Text, password :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

getUserByName :: Connection -> Text -> IO (Maybe User)
getUserByName conn name = do
  uid <- getUserIdByName conn name
  case uid of
    Just i  -> getUserById conn i
    Nothing -> pure Nothing

checkCreds
  :: CookieSettings
  -> JWTSettings
  -> Pool Connection
  -> Login --password
  -> Handler
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
checkCreds cookieSettings jwtSettings pool (Login un pw) = do
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
  {-let usr = User "Ali Baba" "ali@email.com"-}

  -- check password against database, returns :: Maybe session
  authResult <- liftIO . withResource pool $ \c ->
    authUser c un (PasswordPlain pw) (60 * 60)
  case authResult of
    Nothing      -> throwError err401
    Just session -> do
      usr <-
        liftIO $ fromJust <$> (withResource pool $ \c -> getUserByName c un)
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      return $ (fromJust mApplyCookies) NoContent
checkCreds _ _ _ _ = throwError err401
