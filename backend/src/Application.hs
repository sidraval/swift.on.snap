{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Api.Core
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _api :: Snaplet Api
    }

makeLenses ''App


------------------------------------------------------------------------------
type AppHandler = Handler App App
