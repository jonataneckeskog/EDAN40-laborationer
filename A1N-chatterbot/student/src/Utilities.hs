module Utilities where

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
-- Like a normal map, except we iterate over Maybe. When we hit Nothing we don't change it.
-- If we hit a Just x we can actually apply the function f on x.
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a


try :: (a -> Maybe a) -> a -> a
-- We try to change x into a Just y using the function f. If the result is Nothing, we instead keep
-- the original value x. Otherwise we extract the value y and return it.
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

