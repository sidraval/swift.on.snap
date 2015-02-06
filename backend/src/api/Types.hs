{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson
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
  toJSON (User id deviceToken) = object [ "id" .= id, "device_token" .= deviceToken ]

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
  , eventEndedAt   :: Int
  , eventName      :: T.Text
  , eventStartedAt :: Int
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
