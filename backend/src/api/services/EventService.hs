{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.EventService where

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

data EventService = EventService { _pg :: Snaplet Postgres }

makeLenses ''EventService

eventServiceInit :: SnapletInit b EventService
eventServiceInit = makeSnaplet "eventService" "Events service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  return $ EventService pg

instance HasPostgres (Handler b EventService) where
  getPostgresState = with pg get
