{-# LANGUAGE OverloadedStrings #-}

module Api.Services.User where

import Snap.Snaplet

data User = User

userInit :: SnapletInit b User
userInit = makeSnaplet "user" "Users service" Nothing $ do
  return User
