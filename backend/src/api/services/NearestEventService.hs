{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.NearestEventService (NearestEventService(NearestEventService), nearestEventServiceInit) where

import Api.Types
import Api.Utils
import Control.Applicative
import Control.Lens.TH
import Control.Monad.State.Class
import Data.Aeson (encode)
import Data.ByteString.Char8 as B hiding (head, null)
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Core
import Text.Digestive
import Text.Digestive.Snap hiding (method)

data NearestEventService = NearestEventService { _pg :: Snaplet Postgres }

makeLenses ''NearestEventService

nearestEventRoutes :: [(B.ByteString, Handler b NearestEventService ())]
nearestEventRoutes = [("/", method GET (withAuthorizedUser >>= listEvents))]

listEvents :: Maybe User -> Handler b NearestEventService ()
listEvents (Just u) = do
  lat <- getParam "lat"
  lon <- getParam "lon"
  events <- getNearestEvents lat lon
  modifyResponse . setResponseCode $ 200
listEvents _ = unauthorized

getNearestEvents :: Maybe B.ByteString -> Maybe B.ByteString -> Handler b NearestEventService (Maybe [Event])
getNearestEvents (Just lat) (Just lon) = do
  events <- query_ "SELECT * FROM events" :: Handler b NearestEventService [Event]
  return $ Just []
getNearestEvents _ _ = return $ Just []

withAuthorizedUser :: Handler b NearestEventService (Maybe User)
withAuthorizedUser = withAuthorization >>= findOrCreateUser

findOrCreateUser :: Maybe B.ByteString -> Handler b NearestEventService (Maybe User)
findOrCreateUser (Just dt) = do
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt) :: Handler b NearestEventService [User]
  case (safeHead userFromToken) of
    Just u -> return $ Just u
    Nothing -> createUser dt
findOrCreateUser (Nothing) = return Nothing

createUser :: B.ByteString -> Handler b NearestEventService (Maybe User)
createUser dt = do
  newUser <- execute "INSERT INTO users (device_token) VALUES (?)" (Only dt)
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  return $ safeHead userFromToken

nearestEventServiceInit :: SnapletInit b NearestEventService
nearestEventServiceInit = makeSnaplet "nearestEventService" "Nearest events service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  return $ NearestEventService pg

instance HasPostgres (Handler b NearestEventService) where
  getPostgresState = with pg get
