module State where

import Config (Config)
import Control.Concurrent (MVar)
import Data.Text
import Data.Time.Clock.System (SystemTime (systemSeconds))
import Data.Word (Word32)
import DBus.Client (Client)
import Util.DbusNotify (Hints)

type NState = MVar NotificationState

data Notification = Notification
  { nId :: Word32
  , nTimeout :: Word32
  , nTimestamp :: SystemTime
  , notifyType :: Maybe String
  , appName :: Text
  , appIcon :: Text
  , summary :: Text
  , body :: Text
  , hints :: Hints
  , actions :: [Text]
  , widget :: Maybe String
  }
  deriving (Show, Eq)

data NotificationState = NotificationState
  { notifications :: [Notification]
  , config :: Config
  , idCounter :: Word32
  , client :: Client
  }

data Lifetime = Timeout Word32 | Persistent Word32
  deriving (Show, Eq)

data NotificationCloseReason = Expired | Dismiss | CloseNotification | Other
  deriving (Enum)

instance Ord Lifetime where
  (<=) (Timeout _) (Persistent _) = True
  (<=) (Timeout n) (Timeout m) = n Prelude.<= m
  (<=) (Persistent n) (Persistent m) = n >= m
  (<=) _ _ = False

class Transient a where
  lifetime :: a -> Lifetime

instance Transient Notification where
  lifetime n
    | nTimeout n /= 0 =
        Timeout
          ((fromIntegral . systemSeconds . nTimestamp $ n) + nTimeout n)
    | otherwise = Persistent (fromIntegral . systemSeconds . nTimestamp $ n)

updateConfig :: (Config -> Config) -> NotificationState -> NotificationState
updateConfig f state = state{config = f $ config state}

updateNotifications ::
  ([Notification] -> [Notification]) -> NotificationState -> NotificationState
updateNotifications f state = state{notifications = f $ notifications state}

updateIdCounter :: (Word32 -> Word32) -> NotificationState -> NotificationState
updateIdCounter f state = state{idCounter = f $ idCounter state}
