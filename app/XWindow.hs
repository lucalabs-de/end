module XWindow where

import Control.Exception
import qualified Control.Exception.Extensible as Exc
import Control.Monad (foldM)
import Data.Functor
import Data.Maybe (isNothing, listToMaybe, isJust)
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Util.Helpers (flip23, thd)

setupWindow :: IO ()
setupWindow = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt

  ewwWindow <- findSubwindowByName dpy rootw "eww-indicator"
  print ewwWindow

  return ()

findSubwindowByName :: Display -> Window -> String -> IO (Maybe Window)
findSubwindowByName dpy wdw name = do
  wmClassL <-
    (getTextProperty dpy wdw wM_CLASS >>= wcTextPropertyToTextList dpy)
      `Exc.catch` \(Exc.SomeException _) -> return []

  let wmClass = listToMaybe wmClassL

  if wmClass == Just name
    then return $ Just wdw
    else do
      children <- fetchChildren dpy wdw
      foldM
        (\s w -> if isJust s then return s else findSubwindowByName dpy w name)
        Nothing
        children

fetchChildren :: Display -> Window -> IO [Window]
fetchChildren d w = queryTree d w <&> thd
