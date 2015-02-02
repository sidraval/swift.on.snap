{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.UserService where

import Api.Types
import Control.Lens.TH
import Control.Monad.State.Class
import Data.Aeson
import Data.ByteString.Char8 as B
import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Types

data UserService = UserService { _pg :: Snaplet Postgres }

makeLenses ''UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = [("/", method POST createUser)]

createUser :: Handler b UserService ()
createUser = do
  deviceToken <- (getRequest >>= getDeviceToken)
  newUser <- execute "INSERT INTO users (device_token) VALUES (?)" (Only deviceToken)
  modifyResponse $ setResponseStatus 201 "Created"
  writeLBS . encode $ (deviceToken >>= userFromDeviceToken)

userFromDeviceToken :: B.ByteString -> Maybe User
userFromDeviceToken dt = Just $ User 0 (B.unpack dt)

getDeviceToken :: (MonadSnap m) => Request -> m (Maybe ByteString)
getDeviceToken rq = return $ getHeader "device-token" rq

userServiceInit :: SnapletInit b UserService
userServiceInit = makeSnaplet "userService" "Users service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes userRoutes
  return $ UserService pg

instance HasPostgres (Handler b UserService) where
  getPostgresState = with pg get
