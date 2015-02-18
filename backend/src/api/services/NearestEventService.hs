{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.NearestEventService where

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

nearestEventServiceInit :: SnapletInit b NearestEventService
nearestEventServiceInit = makeSnaplet "nearestEventService" "Nearest events service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  return $ NearestEventService pg
