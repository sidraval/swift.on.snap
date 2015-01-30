{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.UserService
import Control.Lens.TH
import Snap.Snaplet

data Api = Api { _userService :: Snaplet UserService }

makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  u <- nestSnaplet "users" userService $ userServiceInit
  return $ Api u
