{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.UserService where

import Api.Types
import Api.Utils
import Control.Applicative
import Control.Lens.TH
import Control.Monad.State.Class
import Data.Aeson
import Data.ByteString.Char8 as B hiding (head, null)
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
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only deviceToken)
  -- TODO: Rendering error json for failures here?
  modifyResponse $ setResponseStatus 201 "Created"
  writeLBS . encode $ (safeHead userFromToken :: Maybe User)

userServiceInit :: SnapletInit b UserService
userServiceInit = makeSnaplet "userService" "Users service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes userRoutes
  return $ UserService pg

instance HasPostgres (Handler b UserService) where
  getPostgresState = with pg get
