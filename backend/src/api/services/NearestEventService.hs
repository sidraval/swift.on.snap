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
import Data.Maybe
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
  radius <- getParam "radius"
  events <- getNearestEvents (toDouble lat) (toDouble lon) (toDouble radius)
  modifyResponse . setResponseCode $ 200
  writeLBS . encode . fromJust $ events
listEvents _ = unauthorized

toDouble :: Maybe B.ByteString -> Maybe Double
toDouble x = x >>= (\y -> return $ (read $ B.unpack y :: Double))

getNearestEvents :: Maybe Double -> Maybe Double -> Maybe Double -> Handler b NearestEventService (Maybe [EventWithDistance])
getNearestEvents (Just lat) (Just lon) (Just rad) = do
  events <- query eventsOrderedByDistance (lat, lon, rad) :: Handler b NearestEventService [EventWithDistance]
  return $ Just events
getNearestEvents _ _ _ = return $ Just []

eventsOrderedByDistance :: Query
eventsOrderedByDistance = "SELECT *, p.distance_unit\
                                     \* DEGREES(ACOS(COS(RADIANS(p.latpoint)) \
                                     \* COS(RADIANS(e.lat)) \
                                     \* COS(RADIANS(p.longpoint) - RADIANS(e.lon)) \
                                     \+ SIN(RADIANS(p.latpoint)) \
                                     \* SIN(RADIANS(e.lat)))) AS distance_in_km \
                                   \FROM events AS e \
                                   \JOIN (SELECT (?) AS latpoint, (?) AS longpoint, \
                                         \(?) AS radius, 111.045 AS distance_unit) \
                                         \AS p ON 1=1 \
                                   \ORDER BY distance_in_km \
                                   \LIMIT 15"

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
  addRoutes nearestEventRoutes
  return $ NearestEventService pg

instance HasPostgres (Handler b NearestEventService) where
  getPostgresState = with pg get
