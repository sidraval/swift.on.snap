{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson
import           Data.Time
import           Snap.Snaplet.PostgresqlSimple

-- User

data User = User
  { id          :: Int
  , deviceToken :: String
  }

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance ToJSON User where
  toJSON (User id deviceToken) = object [ "id" .= show id, "device_token" .= deviceToken ]

-- Attendance
data Attendance = Attendance
  { attendanceId :: Int
  , attendanceEventId :: Int
  , attendanceUserId :: Int
  }

instance FromRow Attendance where
  fromRow = Attendance <$> field
                       <*> field
                       <*> field

instance ToJSON Attendance where
  toJSON (Attendance attendanceId attendanceEventId attendanceUserId) = object [ "id" .= attendanceId
                                                                               , "event_id" .= attendanceEventId
                                                                               , "user_id" .= attendanceUserId
                                                                               ]

-- Event

data Event = Event
  { eventId        :: Int
  , eventEndedAt   :: T.Text
  , eventName      :: T.Text
  , eventStartedAt :: T.Text
  , eventUserId    :: Int
  , eventAddress   :: T.Text
  , eventLat       :: Float
  , eventLon       :: Float
  }

instance FromRow Event where
  fromRow = Event <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToJSON Event where
  toJSON (Event id endedAt name startedAt userId address lat lon) = object [ "id" .= id
                                                                           , "ended_at" .= endedAt
                                                                           , "name" .= name
                                                                           , "started_at" .= startedAt
                                                                           , "user_id" .= userId
                                                                           , "address" .= address
                                                                           , "lat" .= lat
                                                                           , "lon" .= lon
                                                                           ]
-- EventWithDistance

data EventWithDistance = EventWithDistance
  { eventWithDistanceId        :: Int
  , eventWithDistanceEndedAt   :: LocalTime
  , eventWithDistanceName      :: T.Text
  , eventWithDistanceStartedAt :: LocalTime
  , eventWithDistanceUserId    :: Int
  , eventWithDistanceAddress   :: T.Text
  , eventWithDistanceLat       :: Double
  , eventWithDistanceLon       :: Double
  , eventWithDistanceLatPoint  :: Rational
  , eventWithDistanceLonPoint  :: Rational
  , eventWithDistanceRadius    :: Rational
  , eventWithDistanceUnit      :: Rational
  , eventWithDistanceInKm      :: Rational
  }

instance FromRow EventWithDistance where
  fromRow = EventWithDistance <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToJSON EventWithDistance where
  toJSON (EventWithDistance id endedAt name startedAt userId address lat lon _ _ _ _ _) = object [ "id" .= id
                                                                           , "name" .= name
                                                                           , "user_id" .= userId
                                                                           , "address" .= address
                                                                           , "lat" .= lat
                                                                           , "lon" .= lon
                                                                           ]
