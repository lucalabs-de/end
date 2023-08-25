module Main where

import qualified Daemon
import Util.Constants

import Data.ByteString.Char8 (pack)
import Network.Socket (
  Family (AF_UNIX),
  SocketType (Stream),
  close,
  connect,
  socket,
 )
import Network.Socket.ByteString (send)
import System.Environment (getArgs)

sendCommand :: [String] -> IO ()
sendCommand args = do
  sock <- socket AF_UNIX Stream 0
  connect sock ipcSocketAddr
  send sock $ (pack . unwords) args
  close sock

main :: IO ()
main = do
  args <- getArgs
  if null args
    then Daemon.main
    else sendCommand args
