module Util.Constants where

import Network.Socket (SockAddr (..))

ipcSocketAddr :: SockAddr
ipcSocketAddr = SockAddrUnix "/tmp/eww-socket"
