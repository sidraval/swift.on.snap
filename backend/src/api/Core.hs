{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.UserService
import Api.Services.EventService
import Control.Lens.TH
import Snap.Snaplet

data Api = Api { _userService  :: Snaplet UserService
               , _eventService :: Snaplet EventService
               }

makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  u <- nestSnaplet "users" userService userServiceInit
  e <- nestSnaplet "events" eventService eventServiceInit
  return $ Api u e
