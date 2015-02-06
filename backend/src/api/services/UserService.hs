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
import Snap.Core

data UserService = UserService { _pg :: Snaplet Postgres }

makeLenses ''UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = [("/", method POST (withAuthorizedUser >>= create))]

create :: Maybe User -> Handler b UserService ()
create u = writeLBS $ userOrError u

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

-- Auth
withAuthorizedUser :: Handler b UserService (Maybe User)
withAuthorizedUser = withAuthorization >>= findOrCreateUser

findOrCreateUser :: Maybe B.ByteString -> Handler b UserService (Maybe User)
findOrCreateUser (Just dt) = do
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt) :: Handler b UserService [User]
  case (safeHead userFromToken) of
    Just u -> return $ Just u
    Nothing -> createUser dt
findOrCreateUser (Nothing) = return Nothing

createUser :: B.ByteString -> Handler b UserService (Maybe User)
createUser dt = do
  execute "INSERT INTO users (device_token) VALUES (?)" (Only dt)
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  return $ safeHead userFromToken
