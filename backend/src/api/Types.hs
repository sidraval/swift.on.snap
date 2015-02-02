{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson
import           Snap.Snaplet.PostgresqlSimple

data User = User
  { id :: Int
  , deviceToken :: String
  }

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToJSON User where
  toJSON (User id deviceToken) = object [ "id" .= id, "device_token" .= deviceToken ]
