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
import Data.ByteString.Lazy.Internal
import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Types

data UserService = UserService { _pg :: Snaplet Postgres }

makeLenses ''UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = [("/", method POST createIfAuthorized)]

createIfAuthorized :: Handler b UserService ()
createIfAuthorized = do
  deviceToken <- (getRequest >>= getDeviceToken)
  maybe unauthorized authorized deviceToken

authorized :: B.ByteString -> Handler b UserService ()
authorized dt = do
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  maybe (createUser dt) respondWithUser (safeHead userFromToken :: Maybe User)

respondWithUser :: User -> Handler b UserService ()
respondWithUser u = do
  modifyResponse . setResponseCode $ 200
  writeLBS $ userOrError (Just u)

createUser :: B.ByteString -> Handler b UserService ()
createUser dt = do
  newUser <- execute "INSERT INTO users (device_token) VALUES (?)" (Only dt)
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  modifyResponse . setResponseCode $ codeForCreation userFromToken
  writeLBS $ userOrError (safeHead userFromToken :: Maybe User)

userOrError :: Maybe User -> Data.ByteString.Lazy.Internal.ByteString
userOrError user = maybe
  (encode $ object [ "message" .= ("Validation failed" :: String) ])
  encode user

userServiceInit :: SnapletInit b UserService
userServiceInit = makeSnaplet "userService" "Users service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes userRoutes
  return $ UserService pg

instance HasPostgres (Handler b UserService) where
  getPostgresState = with pg get
