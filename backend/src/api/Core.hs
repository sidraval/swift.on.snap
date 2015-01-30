{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.User
import Control.Lens.TH
import Snap.Snaplet

data Api = Api { _user :: Snaplet User }

makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  u <- nestSnaplet "user" user $ userInit
  return $ Api u
