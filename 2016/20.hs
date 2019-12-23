{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Array.Unboxed
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "20"
    s <- map readInput . lines <$> readFile "20.txt"
    let ranges = mkRanges s
    print ranges
    print $ part2 ranges
    
readInput :: String -> (Int,Int)
readInput s = let (a,b) = break (=='-') s in (read a, read $ tail b)

part2 :: [(Int,Int)] -> Int
part2 r = sum
    [ a' - b - 1
    | ((_,b),(a',_)) <- zip r (tail r)
    ]


    
mkRanges :: [(Int,Int)] -> [(Int,Int)]
mkRanges = Data.List.sort . foldr insert []

insert :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
insert ix@(a,b) is =
    let (ins,outs) = Data.List.partition fi is
     in case ins of
            [] -> (a,b):is
            os -> merge ((a,b):ins) : outs
    where
    merge xs = (minimum $ map fst xs, maximum $ map snd xs)
    fi ix'@(a',b')
        = inRange ix' a || inRange ix' b
       || inRange ix a' || inRange ix b'
       