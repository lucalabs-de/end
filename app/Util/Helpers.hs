module Util.Helpers where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bifunctor (second)
import Data.List (minimumBy)
import Data.Aeson.Types (Pair, Object, Value (Object), object)
import Data.Aeson.KeyMap (empty)

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

groupTuples :: [a] -> [(a, a)]
groupTuples [] = []
groupTuples [_] = []
groupTuples (k : v : t) = (k, v) : groupTuples t

-- Aeson Helpers

-- who thought it would be a good idea for Aeson.object to return a value
-- instead of an Object???
asAesonObject :: [Pair] -> Object
asAesonObject p = toObject $ object p
  where toObject (Object obj) = obj
        toObject _ = empty
  
