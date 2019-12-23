{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Lens
import Control.Parallel
import Data.Array
import Data.Maybe
import qualified Data.List
import qualified Data.ByteString.Lazy.Char8 as CLBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Data.Digest.Pure.MD5
import System.Environment


main = do
    putStrLn "15"
    print $ part1 examp
    print $ part1 input
    print $ part1 input2

part1 :: [(Int,Int)] -> Int
part1 = crt . transform1
transform1 :: [(Int,Int)] -> [(Int,Int)]
transform1 is =
    [ (negate (p+i) `mod` ps,ps)
    | (i,(ps,p)) <- zip [1..] is
    ]
    
crt :: [(Int,Int)] -> Int
crt = go . Data.List.sortOn snd
    where
    go [] = 0
    go ((n,m):ts) = filterDown ts [n,n+m..]
    filterDown [] ts = head ts
    filterDown ((n,m):rs) ts = filterDown rs $ filter (\p -> (p `mod` m) == (n `mod` m)) ts

examp = [(5,4),(2,1)]
input = [(17,1),(7 ,0),(19,2),(5 ,0),(3 ,0),(13,5)]
input2 = [(17,1),(7 ,0),(19,2),(5 ,0),(3 ,0),(13,5),(11,0)]

-- Disc #1 has 17 positions; at time=0, it is at position 1.
-- Disc #2 has 7  positions; at time=0, it is at position 0.
-- Disc #3 has 19 positions; at time=0, it is at position 2.
-- Disc #4 has 5  positions; at time=0, it is at position 0.
-- Disc #5 has 3  positions; at time=0, it is at position 0.
-- Disc #6 has 13 positions; at time=0, it is at position 5.