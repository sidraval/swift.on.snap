{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.AttendanceService where

import Api.Types
import Api.Utils
import Control.Applicative
import Control.Lens.TH
import Control.Monad.State.Class
import Data.Aeson (encode)
import Data.ByteString.Char8 as B hiding (head, null)
import Data.ByteString.Lazy.Internal
import Data.Maybe
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Types
import Text.Digestive
import Text.Digestive.Snap hiding (method)

data AttendanceService = AttendanceService { _pg :: Snaplet Postgres }

makeLenses ''AttendanceService

attendanceRoutes :: [(B.ByteString, Handler b AttendanceService ())]
attendanceRoutes = [("/", method POST (withAuthorizedUser >>= createAttendance))]

createAttendance :: Maybe User -> Handler b AttendanceService ()
createAttendance (Just u) = do
  (view, result) <- runForm "attendance" (attendanceForm $ Api.Types.id u)
  case result of
    Just x -> validAttendance x
    Nothing -> modifyResponse . setResponseCode $ 500
createAttendance _ = unauthorized

validAttendance :: Attendance -> Handler b AttendanceService ()
validAttendance a = do
  execute "INSERT INTO attendances (user_id, event_id) VALUES (?, ?)" (attendanceUserId a, attendanceEventId a)
  modifyResponse . setResponseCode $ 201
  writeLBS . encode $ a

attendanceForm :: (Monad m) => Int -> Form T.Text m Attendance
attendanceForm userId = Attendance <$> "attendanceId"      .: pure 0
                                   <*> "attendanceEventId" .: stringRead "" Nothing
                                   <*> "attendanceUserId"  .: pure userId

attendanceServiceInit :: SnapletInit b AttendanceService
attendanceServiceInit = makeSnaplet "attendanceService" "Attendances service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes attendanceRoutes
  return $ AttendanceService pg

instance HasPostgres (Handler b AttendanceService) where
  getPostgresState = with pg get

-- Auth
withAuthorizedUser :: Handler b AttendanceService (Maybe User)
withAuthorizedUser = withAuthorization >>= findOrCreateUser

findOrCreateUser :: Maybe B.ByteString -> Handler b AttendanceService (Maybe User)
findOrCreateUser (Just dt) = do
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt) :: Handler b AttendanceService [User]
  case (safeHead userFromToken) of
    Just u -> return $ Just u
    Nothing -> createUser dt
findOrCreateUser (Nothing) = return Nothing

createUser :: B.ByteString -> Handler b AttendanceService (Maybe User)
createUser dt = do
  newUser <- execute "INSERT INTO users (device_token) VALUES (?)" (Only dt)
  userFromToken <- query "SELECT * FROM users WHERE device_token = (?) LIMIT 1" (Only dt)
  return $ safeHead userFromToken
