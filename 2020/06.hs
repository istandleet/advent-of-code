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
    s <- readFile "06.txt"
    let input = readboard s
    print $ part1 input
    print $ part2 input

part1 :: [[String]] -> Int
part1 = sum . map length . map (mconcat . map Set.fromList)

part2 :: [[String]] -> Int
part2 = sum . map length . map (foldl1 Set.intersection . map Set.fromList)

readboard = uncurry (:) . swap . foldr go ([],[]) . lines
    where go l (r,acc) = if null l then (acc:r,[]) else (r,l:acc) 