{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

main :: IO ()
main = do
    input <- readFile "04.txt" :: IO String
    let plays = filter (not . null) $ lines input
    print $ numOverlappingRanges $ map parseRange plays
    print $ numOverlappingRanges' $ map parseRange plays

data Range = Range { lowerBound :: Int, upperBound :: Int }

parseRange :: String -> (Range, Range)
parseRange str = (Range { lowerBound = read lowerBound1, upperBound = read upperBound1 },
                  Range { lowerBound = read lowerBound2, upperBound = read upperBound2 })
  where [rangeStr1, rangeStr2] = map (\s -> filter (/= ' ') s) $ splitOn ',' str
        [lowerBound1, upperBound1] = words $ map (\c -> if c == '-' then ' ' else c) rangeStr1
        [lowerBound2, upperBound2] = words $ map (\c -> if c == '-' then ' ' else c) rangeStr2

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list = first : splitOn delimiter rest
  where (first, rest') = break (== delimiter) list
        rest = if null rest' then [] else tail rest'

numOverlappingRanges :: [(Range, Range)] -> Int
numOverlappingRanges rangePairs = countOverlapping rangePairs 0
  where countOverlapping [] count = count
        countOverlapping ((r1, r2):rest) count =
          countOverlapping rest (count + (if lowerBound r1 <= lowerBound r2 && upperBound r1 >= upperBound r2
                                           then 1
                                           else if lowerBound r2 <= lowerBound r1 && upperBound r2 >= upperBound r1
                                                 then 1
                                                 else 0))

numOverlappingRanges' :: [(Range, Range)] -> Int
numOverlappingRanges' rangePairs = countOverlapping rangePairs 0
  where countOverlapping [] count = count
        countOverlapping ((r1, r2):rest) count =
          countOverlapping rest (count + (if upperBound r1 >= lowerBound r2 && upperBound r2 >= lowerBound r1
                                           then 1
                                           else 0))