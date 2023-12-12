{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Lens

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Time
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, D)

main = do
    input <- readInput <$> readFile "09.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 = sum . map p1row
part2 = sum . map p2row

p1row xs = extrapolate (residue xs) (length xs)
p2row = foldr (-) 0 . residue

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

residue :: [Int] -> [Int] -- given a list of ints, get the first element of all the diffs
residue = go
    where
    go [] = []
    go xs
        | all (==0) xs = []
        | otherwise = head xs : go (diffs xs)

extrapolate :: [Int] -> Int -> Int -- given a residue, return the polynomial generated by it
extrapolate xs = go
    where
    go n = sum $ zipWith (*) xs $ map (choose n) [0..]
    raise n p = max n 0 ^ p

extrapolate' xs = go
    where
    go n = zipWith (,) (take (n+1) xs) $ zipWith choose [n,n..] [0..]

type Input = [[Int]]
readInput :: String -> Input
readInput = map (map read . words) . filter (not . null) . lines


-- choose :: Integer -> Integer -> Integer
n `choose` k = product [1+max k (n-k)..n] `div` product [1..min k (n-k)]