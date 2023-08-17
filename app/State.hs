module State where

import Control.Concurrent (MVar, modifyMVar)
import Control.Monad.State (State, runState)
import Data.Int (Int32)
import Data.Tuple (swap)
import Data.Word (Word32)

data Notification = Notification
  { timeout :: Int32
  , id :: Word32
  , notifyType :: String
  }

newtype NotificationState = NotificationState
  {notifications :: [Notification]}
