{-# LANGUAGE OverloadedStrings #-}
module Api.Utils where

import Data.ByteString.Char8 as B hiding (head, null)
import Data.Maybe
import Snap.Types

getDeviceToken :: (MonadSnap m) => Request -> m (Maybe B.ByteString)
getDeviceToken rq = return $ getHeader "device-token" rq

safeHead :: [a] -> Maybe a
safeHead as
  | null as = Nothing
  | otherwise = Just $ head as
