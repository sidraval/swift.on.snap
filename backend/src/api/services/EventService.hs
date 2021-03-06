{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.EventService where

import Api.Services.NearestEventService
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

data EventService = EventService { _pg :: Snaplet Postgres 
                                 , _nearestEventService :: Snaplet NearestEventService
                                 }

makeLenses ''EventService

eventRoutes :: [(B.ByteString, Handler b EventService ())]
eventRoutes = [("/", method POST (withAuthorizedUser >>= createEvent))]

createEvent :: Maybe User -> Handler b EventService ()
createEvent (Just u) = do
  (view, result) <- runForm "event" (eventForm $ Api.Types.id u)
  case result of
    Just x -> validEvent x
    Nothing -> modifyResponse . setResponseCode $ 500
createEvent _ = unauthorized

validEvent :: Event -> Handler b EventService ()
validEvent e = do
  newEvent <- execute "INSERT INTO events (ended_at, name, started_at, user_id, address, lat, lon) VALUES (?, ?, ?, ?, ?, ?, ?)" (eventEndedAt e, eventName e, eventStartedAt e, eventUserId e, eventAddress e, eventLat e, eventLon e)
  modifyResponse . setResponseCode $ 202
  writeLBS . encode $ e

eventForm :: (Monad m) => Int -> Form T.Text m Event
eventForm userId = Event
  <$> "eventId" .: pure 0
  <*> "eventEndedAt" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventName" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventStartedAt" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventUserId" .: pure userId
  <*> "eventAddress" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventLat" .: stringRead "" Nothing
  <*> "eventLon" .: stringRead "" Nothing

withAuthorizedUser :: Handler b EventService (Maybe User)
withAuthorizedUser = withAuthorization >>= findOrCreateUser

findOrCreateUser :: Maybe B.ByteString -> Handler b EventService (Maybe User)
findOrCreateUser (Just dt) = do
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt) :: Handler b EventService [User]
  case (safeHead userFromToken) of
    Just u -> return $ Just u
    Nothing -> createUser dt
findOrCreateUser (Nothing) = return Nothing

createUser :: B.ByteString -> Handler b EventService (Maybe User)
createUser dt = do
  newUser <- execute "INSERT INTO users (device_token) VALUES (?)" (Only dt)
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  return $ safeHead userFromToken

eventServiceInit :: SnapletInit b EventService
eventServiceInit = makeSnaplet "eventService" "Events service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  nes <- nestSnaplet "nearests" nearestEventService nearestEventServiceInit
  addRoutes eventRoutes
  return $ EventService pg nes

instance HasPostgres (Handler b EventService) where
  getPostgresState = with pg get
