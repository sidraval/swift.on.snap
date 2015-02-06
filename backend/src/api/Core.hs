{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.UserService
import Api.Services.EventService
import Api.Services.AttendanceService
import Control.Lens.TH
import Snap.Snaplet

data Api = Api { _userService  :: Snaplet UserService
               , _eventService :: Snaplet EventService
               , _attendanceService :: Snaplet AttendanceService
               }

makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  u <- nestSnaplet "users" userService userServiceInit
  e <- nestSnaplet "events" eventService eventServiceInit
  a <- nestSnaplet "attendances" attendanceService attendanceServiceInit
  return $ Api u e a
