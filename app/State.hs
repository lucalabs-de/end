module State where

import Control.Concurrent (MVar, modifyMVar)
import Control.Monad.State (State, runState)
import Data.Tuple (swap)
import Data.Word (Word32)
import Data.Text

data Notification = Notification
  { timeout :: Int
  , nId :: Word32
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
