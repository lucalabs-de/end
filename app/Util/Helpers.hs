module Util.Helpers where

thd :: (a, b, c) -> c
thd (_, _, c) = c

flip23 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip23 f a c b = f a b c
