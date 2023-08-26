module State where

import Control.Concurrent (MVar, modifyMVar)
import Control.Monad.State (State, runState)
import Data.Text
import Data.Tuple (swap)
import Data.Word (Word32)

data Notification = Notification
  { nId :: Word32
  , nTimeout :: Word32
  , notifyType :: Maybe String
  , appName :: Text
  , appIcon :: Text
  , summary :: Text
  , body :: Text
  , hintString :: String
  , widget :: Maybe String
  }

newtype NotificationState = NotificationState
  {notifications :: [Notification]}

type NState = MVar NotificationState
