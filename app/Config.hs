{-# LANGUAGE LambdaCase #-}

module Config where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import System.Directory (
  XdgDirectory (XdgConfig),
  doesFileExist,
  getXdgDirectory,
 )
import System.FilePath (joinPath)
import Toml (Result (..), decode)
import Toml.FromValue (
  FromValue (fromValue),
  ParseTable,
  optKey,
  parseTableFromValue,
  reqKey,
 )

defaultConfig :: Config
defaultConfig =
  Config
    { ewwDefaultNotificationKey = Nothing
    , ewwWindow = Nothing
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

newtype ConfigFile = ConfigFile {config :: Config}

data Config = Config
  { ewwDefaultNotificationKey :: Maybe String
  , ewwWindow :: Maybe EwwWindow
  , maxNotifications :: Word32
  , notificationOrientation :: Orientation
  , timeout :: Timeout
  }
  deriving (Eq, Show)

type EwwWindow = String

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

instance FromValue ConfigFile where
  fromValue = parseTableFromValue (ConfigFile <$> optKeyWithDefault "config" defaultConfig)

instance FromValue Config where
  fromValue =
    parseTableFromValue
      ( Config
          <$> optKey "eww-default-notification-key"
          <*> optKey "eww-window"
          <*> optKeyWithDefault
            "max-notifications"
            (maxNotifications defaultConfig)
          <*> ( \case
                  Just "v" -> Vertical
                  Just "h" -> Horizontal
                  Nothing -> notificationOrientation defaultConfig
                  _invalidValue -> notificationOrientation defaultConfig
                  <$> optKey "notification-orientation"
              )
          <*> optKeyWithDefault "timeout" (timeout defaultConfig)
      )

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
    dTimeout = defaultConfig // timeout // byUrgency
    lowTimeout = low dTimeout
    normalTimeout = normal dTimeout
    criticalTimeout = critical dTimeout

optKeyWithDefault :: (FromValue a) => String -> a -> ParseTable a
optKeyWithDefault k v = fromMaybe v <$> optKey k

prettyPrintParserOutput :: [String] -> IO ()
prettyPrintParserOutput = mapM_ putStrLn

prettyPrintParserError :: [String] -> IO ()
prettyPrintParserError e = putStrLn "There were errors your config.toml! \n" >> prettyPrintParserOutput e

prettyPrintParserWarning :: [String] -> IO ()
prettyPrintParserWarning e = putStrLn "Warning:" >> prettyPrintParserOutput e

importConfig :: IO (Maybe Config)
importConfig =
  do
    configDir <- getXdgDirectory XdgConfig ""
    let configFile = joinPath [configDir, "end", "config.toml"]
    exists <- doesFileExist configFile

    if exists
      then do
        configStr <- readFile configFile
        let parsedFile = decode configStr :: Result String ConfigFile

        case parsedFile of
          Success w cfg ->
            do
              unless (null w) (prettyPrintParserWarning w)
              return $ Just (config cfg)
          Failure e -> do
            prettyPrintParserError e
            return Nothing
      else do
        putStrLn $ "could not find config file! should be at " ++ configFile
        return Nothing

-- convenience function to access config fields
(//) :: a -> (a -> b) -> b
(//) r f = f r

infixl 9 //
