module Util.CliParsers where

import Data.Version (showVersion)
import Options.Applicative (
  Parser,
  command,
  execParser,
  header,
  help,
  helper,
  hsubparser,
  info,
  infoOption,
  long,
  metavar,
  optional,
  progDesc,
  strArgument,
 )
import Paths_EwwNotificationDaemon (version)

newtype Options = Options {optCommand :: Maybe Command}

data Command = Close CloseOptions | Action ActionOptions | Stop
newtype CloseOptions = CloseOptions {nId :: String}
data ActionOptions = ActionOptions {notificationId :: String, actionKey :: String}

commandParser :: Parser (Maybe Command)
commandParser =
  optional $
    hsubparser $
      command "stop" (info stopCommand (progDesc "Stop the notification daemon"))
        <> command "close" (info closeCommand (progDesc "Close a notification"))
        <> command "action" (info actionCommand (progDesc "Invoke an action"))

stopCommand :: Parser Command
stopCommand = pure Stop

closeCommand :: Parser Command
closeCommand = Close <$> closeOptionsParser

actionCommand :: Parser Command
actionCommand = Action <$> actionOptionsParser

closeOptionsParser :: Parser CloseOptions
closeOptionsParser =
  CloseOptions
    <$> strArgument
      (metavar "ID" <> help "ID of the notification that should be closed")

actionOptionsParser :: Parser ActionOptions
actionOptionsParser =
  ActionOptions
    <$> strArgument
      (metavar "ID" <> help "ID of the notification with the action")
    <*> strArgument
      (metavar "key" <> help "Key of the action that should be invoked")

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

versionOption :: Parser (a -> a)
versionOption = infoOption ("end " ++ showVersion version) (long "version" <> help "Show version")

getCliOptions :: IO Options
getCliOptions =
  execParser $
    info (helper <*> versionOption <*> optionsParser) $
      header ("end - eww notification daemon " ++ showVersion version)
