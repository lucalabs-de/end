module Util.Constants where

import Network.Socket (SockAddr (..))
import System.FilePath ((</>))

ipcSocketAddr :: SockAddr
ipcSocketAddr = SockAddrUnix "/tmp/eww-socket"

imageTempDir :: FilePath
imageTempDir = "/tmp" </> "end-images"
