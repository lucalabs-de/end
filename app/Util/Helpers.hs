module Util.Helpers where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad (void)

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
