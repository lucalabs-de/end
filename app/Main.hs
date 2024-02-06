module Main where

import qualified Daemon
import Util.CliParsers
import Util.Constants

import Control.Exception (onException)
import Data.ByteString.Char8 (pack)
import Network.Socket (
  Family (AF_UNIX),
  SocketType (Stream),
  close,
  connect,
  socket,
 )
import Network.Socket.ByteString (send)

writeIpcSocket :: [String] -> IO ()
writeIpcSocket args =
  onException
    ( do
        sock <- socket AF_UNIX Stream 0
        connect sock ipcSocketAddr
        _ <- send sock $ (pack . unwords) args
        close sock
    )
    (putStrLn "daemon is not running, run end first!")

sendCommand :: Command -> IO ()
sendCommand Stop = writeIpcSocket ["kill"]
sendCommand (Close closeOps) = writeIpcSocket ["close", nId closeOps]
sendCommand (Action actionOps) = writeIpcSocket ["action", notificationId actionOps ++ " " ++ actionKey actionOps]

main :: IO ()
main = do
  opts <- getCliOptions
  maybe Daemon.main sendCommand (optCommand opts)
