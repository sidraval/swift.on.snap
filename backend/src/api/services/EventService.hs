{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.EventService where

import Api.Types
import Api.Utils
import Control.Applicative
import Control.Lens.TH
import Control.Monad.State.Class
{- import Data.Aeson -}
import Data.ByteString.Char8 as B hiding (head, null)
import Data.ByteString.Lazy.Internal
import Data.Maybe
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Types
import Text.Digestive
import Text.Digestive.Snap hiding (method)

data EventService = EventService { _pg :: Snaplet Postgres }

makeLenses ''EventService

eventRoutes :: [(B.ByteString, Handler b EventService ())]
eventRoutes = [("/", method POST (withAuthorization >>= createEvent))]

createEvent :: Maybe B.ByteString -> Handler b EventService ()
createEvent (Just dt) = do
  (view, result) <- runForm "event" eventForm
  case result of
    Just x -> modifyResponse . setResponseCode $ 201
    Nothing -> modifyResponse . setResponseCode $ 500
createEvent _ = unauthorized

eventForm :: (Monad m) => Form T.Text m Event
eventForm = Event
  <$> "eventId" .: stringRead "" Nothing
  <*> "eventEndedAt" .: stringRead "" Nothing
  <*> "eventName" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventStartedAt" .: stringRead "" Nothing
  <*> "eventUserId" .: stringRead "" Nothing
  <*> "eventAddress" .: check "Not Empty" isNotEmpty (text Nothing)
  <*> "eventLat" .: stringRead "" Nothing
  <*> "eventLon" .: stringRead "" Nothing

eventServiceInit :: SnapletInit b EventService
eventServiceInit = makeSnaplet "eventService" "Events service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes eventRoutes
  return $ EventService pg

instance HasPostgres (Handler b EventService) where
  getPostgresState = with pg get
