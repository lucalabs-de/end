module Util.Helpers where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Bifunctor (second)

data Permit = Permit
type Barrier = MVar Permit

unlockBarrier :: Barrier -> IO ()
unlockBarrier barrier = putMVar barrier Permit

waitAtBarrier :: Barrier -> IO ()
waitAtBarrier = void . takeMVar

thd :: (a, b, c) -> c
thd (_, _, c) = c

flip23 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip23 f a c b = f a b c

getMax :: Ord b => (a -> b) -> b -> [a] -> b
getMax f = foldr (\n c -> max c (f n))

tuple :: a -> (a, a)
tuple v = (v, v)

replaceOrPrepend :: (a -> Bool) -> a -> [a] -> [a]
replaceOrPrepend f e l = if fst replaceResult then snd replaceResult else e : l
  where replaceResult = tryReplace f e l

-- Tries to replace the first element that matches the predicate.
-- Returns (True, l) where l is the new list if an element was replaced
-- and (False, l) otherwise.
tryReplace :: (a -> Bool) -> a -> [a] -> (Bool, [a])
tryReplace f e [] = (False, [])
tryReplace f e (h : l) = if f h then (True, e : l) else second (h :) (tryReplace f e l)
