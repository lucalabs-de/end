{-# LANGUAGE LambdaCase #-}

module Config where

import Control.Exception (onException)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath (joinPath)
import Toml (Result (..), decode)
import Toml.FromValue (FromValue (fromValue), ParseTable, optKey, parseTableFromValue, reqKey)

defaultConfig :: Config
defaultConfig =
  Config
    { settings =
        Settings
          { ewwContentKey = ""
          , ewwDefaultNotificationKey = Nothing
          , maxNotifications = 0
          , notificationOrientation = Vertical
          , timeout =
              Timeout
                { byUrgency =
                    TimeoutByUrgency
                      { low = 5
                      , normal = 10
                      , critical = 0
                      }
                }
          }
    , customNotifications = []
    }

data Orientation = Horizontal | Vertical
  deriving (Eq, Show)

newtype Timeout = Timeout {byUrgency :: TimeoutByUrgency}
  deriving (Eq, Show)

data TimeoutByUrgency = TimeoutByUrgency
  { low :: Word32
  , normal :: Word32
  , critical :: Word32
  }
  deriving (Eq, Show)

data CustomNotification = CustomNotification
  { name :: String
  , ewwKey :: String
  , hint :: String
  , customTimeout :: Word32
  }
  deriving (Eq, Show)

data Settings = Settings
  { ewwContentKey :: String
  , ewwDefaultNotificationKey :: Maybe String
  , maxNotifications :: Word32
  , notificationOrientation :: Orientation
  , timeout :: Timeout
  }
  deriving (Eq, Show)

data Config = Config
  { settings :: Settings
  , customNotifications :: [CustomNotification]
  }
  deriving (Eq, Show)

instance FromValue Config where
  fromValue =
    parseTableFromValue
      ( Config
          <$> reqKey "config"
          <*> optKeyWithDefault
            "notification-type"
            (customNotifications defaultConfig)
      )

instance FromValue Settings where
  fromValue =
    parseTableFromValue
      ( Settings
          <$> reqKey "eww-content-key"
          <*> optKey "eww-default-notification-key"
          <*> optKeyWithDefault
            "max-notifications"
            (maxNotifications defaultSettings)
          <*> ( \case
                  Just "v" -> Vertical
                  Just "h" -> Horizontal
                  Nothing -> notificationOrientation defaultSettings
                  _ -> Vertical
                  <$> optKey "notification-orientation"
              )
          <*> optKeyWithDefault "timeout" (timeout defaultSettings)
      )
   where
    defaultSettings = settings defaultConfig

instance FromValue CustomNotification where
  fromValue =
    parseTableFromValue
      ( CustomNotification
          <$> reqKey "name"
          <*> reqKey "eww-key"
          <*> reqKey "hint"
          <*> optKeyWithDefault "timeout" 0
      )

-- instance FromValue Orientation where
--   fromValue =
--     parseTableFromValue
--       ( \case
--           Just "v" -> Vertical
--           Just "h" -> Horizontal
--           Nothing -> notificationOrientation defaultConfig
--           _ -> Vertical
--           <$> optKey "notification-orientation"
--       )

instance FromValue Timeout where
  fromValue = parseTableFromValue (Timeout <$> reqKey "urgency")

instance FromValue TimeoutByUrgency where
  fromValue =
    parseTableFromValue
      ( TimeoutByUrgency
          <$> optKeyWithDefault "low" lowTimeout
          <*> optKeyWithDefault "normal" normalTimeout
          <*> optKeyWithDefault "critical" criticalTimeout
      )
   where
    dTimeout = byUrgency . timeout . settings $ defaultConfig
    lowTimeout = low dTimeout
    normalTimeout = normal dTimeout
    criticalTimeout = critical dTimeout

optKeyWithDefault :: FromValue a => String -> a -> ParseTable a
optKeyWithDefault k v = fromMaybe v <$> optKey k

importConfig :: IO (Maybe Config)
importConfig =
  onException
    ( do
        configDir <- getXdgDirectory XdgConfig ""
        let configFile = joinPath [configDir, "end", "config.toml"]

        configStr <- readFile configFile
        let config = decode configStr :: Result String Config

        case config of
          Success w cfg ->
            do
             print w
             return $ Just cfg
          Failure e -> do
            print e
            return Nothing
    )
    ( do
        putStrLn "config parsing failed"
        return Nothing
    )
