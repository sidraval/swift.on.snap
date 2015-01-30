{-# LANGUAGE OverloadedStrings #-}

module Api.Core where

import Api.Services.User
import Snap.Snaplet

data Api = Api { _user :: Snaplet User }
