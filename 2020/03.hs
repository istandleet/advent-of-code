{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language RecordWildCards #-}
module Main where

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    s <- readFile "03.txt"
    let input = readBoard s
    print $ part1 input
    print $ part1' (3,1) input
    print $ part2 input

part1 :: Board -> Int
part1 board = length $ filter id $ map (board!) $ p1indices board

p1indices :: Board -> [Coord]
p1indices board = map (\y -> ((3*y) `mod` (xmax+1), y)) [0..ymax]
    where (xmax,ymax) = snd $ bounds board

part1' :: Slope -> Board -> Int
part1' slope board = length $ filter id $ map (board!) $ p2indices slope board

part2 :: Board -> Int
part2 board = product $ map (\s -> part1' s board) slopes
    where
    slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

p2indices :: Slope -> Board -> [Coord]
p2indices (mx, my) board = takeWhile (inRange bnds) $ map (\i -> ((mx*i) `mod` (xmax+1), my*i)) [0..]
    where 
    bnds = bounds board
    xmax = fst $ snd bnds

type Slope = (Int,Int)
type Coord = (Int,Int)
type Board = UArray Coord Bool

-- Parsing
readBoard :: String -> Board
readBoard s = array bnds 
    [ ((x,y), c == '#')
    | (y,l) <- zip [0..] $ filter (not.null) $ lines s
    , (x,c) <- zip [0..] l
    ]
    where 
    bnds = ((0,0),(xmax,ymax))
    xmax = pred $ length $ head $ lines s
    ymax = pred $ length $ filter (not.null) $ lines s


readBoard' s = 
    [ ((x,y), c == '#')
    | (y,l) <- zip [0..] $ filter (not.null) $ lines s
    , (x,c) <- zip [0..] l
    ]

readBoard'' s = bnds
    where 
    bnds = ((0,xmax),(0,ymax))
    xmax = pred $ length $ head $ lines s
    ymax = pred $ length $ filter (not.null) $ lines s
    