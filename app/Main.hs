module Main where

import qualified Daemon
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Network.Socket
import Network.Socket.ByteString
import System.Environment (getArgs)
import Util.Constants (ipcSocketAddr)

sendCommand :: [String] -> IO ()
sendCommand args = do
  sock <- socket AF_UNIX Stream 0
  connect sock ipcSocketAddr
  send sock $ (pack . intercalate "") args
  close sock

main :: IO ()
main = do
  args <- getArgs
  if null args
    then Daemon.main
    else sendCommand args
