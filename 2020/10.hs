{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    s <- readFile "10.txt"
    let input = map read $ filter (not.null) $ lines s
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Int
part1 = uncurry (*) . count1and3 . (\l -> zipWith (-) (tail l) l) . cleanList
    where count1and3 l = (length $ filter (==1) l, length $ filter (==3) l)

cleanList :: [Int] -> [Int]
cleanList = (\l -> l ++ [last l + 3]) . (0:) . Data.List.sort

part2 :: [Int] -> Int
part2 = (! 0) . makePaths . cleanList

makePaths :: [Int] -> Array Int Int
makePaths l = arr
    where
    arr = array bnds
        [ (i, go i)
        | i <- range bnds
        ]
    bnds = (minimum l, maximum l)
    sl = IntSet.fromList l
    go i
      | i == snd bnds = 1
      | i `IntSet.notMember` sl = 0
      | otherwise = sum $ map (arr !) $ filter (inRange bnds) $ [i+1, i+2, i+3]
    