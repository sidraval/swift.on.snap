{-# LANGUAGE OverloadedStrings #-}

module Api.Services.UserService where

import Snap.Snaplet

data UserService = UserService

userServiceInit :: SnapletInit b UserService
userServiceInit = makeSnaplet "userService" "Users service" Nothing $ do
  return UserService
