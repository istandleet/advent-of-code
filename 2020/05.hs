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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    s <- readFile "05.txt"
    let input = map readseat $ filter (not.null) $ lines s
    print $ part1 input
    print $ part2 input

part1 :: [Seat] -> Int
part1 = maximum . map seatid

part2 :: [Seat] -> Int
part2 taken = 
    let ids = IntSet.fromList $ map seatid taken
     in head 
      [ seat
      | seat <- [IntSet.findMin ids..IntSet.findMax ids]
      , seat `IntSet.notMember` ids
      , (seat+1) `IntSet.member` ids
      , (seat-1) `IntSet.member` ids
      ]

seatid (a,b) = a*8+b

type Seat = (Int,Int)

readseat :: String -> Seat
readseat = bimap front side . splitAt 7
    
front :: String -> Int
front = foldl (\acc c -> if c == 'B' then acc*2+1 else acc*2) 0
side :: String -> Int
side = foldl (\acc c -> if c == 'R' then acc*2+1 else acc*2) 0
