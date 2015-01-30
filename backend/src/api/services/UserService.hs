{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.UserService where

import Control.Lens.TH
import Control.Monad.State.Class
import Data.ByteString.Char8 as B
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Types

data UserService = UserService { _pg :: Snaplet Postgres }

makeLenses ''UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = []

userServiceInit :: SnapletInit b UserService
userServiceInit = makeSnaplet "userService" "Users service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes userRoutes
  return $ UserService pg

instance HasPostgres (Handler b UserService) where
  getPostgresState = with pg get
