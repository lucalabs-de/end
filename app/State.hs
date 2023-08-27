module State where

import Config (Config)
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

data NotificationState = NotificationState
  { notifications :: [Notification]
  , config :: Config
  }

type NState = MVar NotificationState

-- TODO: Maybe make this general using lens?

updateConfig :: (Config -> Config) -> NotificationState -> NotificationState
updateConfig f state = state{config = f $ config state}

updateNotifications ::
  ([Notification] -> [Notification]) -> NotificationState -> NotificationState
updateNotifications f state = state{notifications = f $ notifications state}
