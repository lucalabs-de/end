module State where

import Control.Concurrent (MVar, modifyMVar)
import Control.Monad.State (State, runState)
import Data.Int (Int32)
import Data.Tuple (swap)
import Data.Word (Word32)
import Data.Text

data Notification = Notification
  { timeout :: Int32
  , nId :: Word32
  , notifyType :: Maybe String
  , appName :: Text
  , appIcon :: Text
  , summary :: Text
  , body :: Text
  , hintString :: String
  }

newtype NotificationState = NotificationState
  {notifications :: [Notification]}
