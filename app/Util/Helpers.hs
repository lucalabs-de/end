module Util.Helpers where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Bifunctor (second)
import Data.List (minimumBy)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))

data Permit = Permit
type Barrier = MVar Permit

-- General Util
thd :: (a, b, c) -> c
thd (_, _, c) = c

flip23 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip23 f a c b = f a b c

tuple :: a -> (a, a)
tuple v = (v, v)

minWith :: (Ord b) => (a -> b) -> [a] -> a
minWith f = minimumBy (\a b -> compare (f a) (f b))

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- Async
unlockBarrier :: Barrier -> IO ()
unlockBarrier barrier = putMVar barrier Permit

waitAtBarrier :: Barrier -> IO ()
waitAtBarrier = void . takeMVar

-- List operations
replaceNewlines :: String -> String
replaceNewlines = map (\c -> if c == '\n' then ' ' else c)

replaceOrPrepend :: (a -> Bool) -> a -> [a] -> [a]
replaceOrPrepend f e l = if fst replaceResult then snd replaceResult else e : l
 where
  replaceResult = tryReplace f e l

-- Tries to replace the first element that matches the predicate.
-- Returns (True, l) where l is the new list if an element was replaced
-- and (False, l) otherwise.
tryReplace :: (a -> Bool) -> a -> [a] -> (Bool, [a])
tryReplace _ _ [] = (False, [])
tryReplace f e (h : l) = if f h then (True, e : l) else second (h :) (tryReplace f e l)

cnv :: [a] -> [(a, a)]
cnv [] = []
cnv [x] = []
cnv (k:v:t) = (k, v) : cnv t
