{-# LANGUAGE OverloadedStrings #-}
module Api.Utils where

import Data.ByteString.Char8 as B hiding (head, null)
import Data.Maybe
import Snap.Snaplet
import Snap.Types

withAuthorization :: (MonadSnap m) => m (Maybe B.ByteString)
withAuthorization = getRequest >>= getDeviceToken

getDeviceToken :: (MonadSnap m) => Request -> m (Maybe B.ByteString)
getDeviceToken rq = return $ getHeader "device-token" rq

unauthorized :: Handler b v ()
unauthorized = modifyResponse $ setResponseCode 401

safeHead :: [a] -> Maybe a
safeHead as
  | null as = Nothing
  | otherwise = Just $ head as

codeForCreation :: [a] -> Int
codeForCreation as
  | null as = 422
  | otherwise = 201
