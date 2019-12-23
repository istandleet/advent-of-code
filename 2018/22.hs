module Main where 

import Data.Array.Unboxed

main = do
    print $ sum $ elems rls


target = (13,726)
depth = 3066
-- target = (10,10)
-- depth = 510



rls :: Array (Int,Int) Int
rls = amap (`mod` 3) els

els :: Array (Int,Int) Int
els = amap (\gi -> (gi + depth) `mod` 20183) gis

gis :: Array (Int,Int) Int
gis = array bnds [((x,y),go x y) | (x,y) <- range bnds] 
  where
  bnds = ((0,0),target)
  go 0 0 = 0
  go x 0 = x * 16807
  go 0 y = y * 48271
  go x y = if (x,y) == target then 0 else els !(x-1,y) * els !(x,y-1)