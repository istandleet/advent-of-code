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
    input <- readInput <$> readFile "06.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 = product . map (uncurry waysToWin')
part2 input = 
    let (time, distance) = unzip input 
        time' = read $ foldMap show time
        distance' = read $ foldMap show distance
     in waysToWin' time' distance'

waysToWin :: Int -> Int -> Int
waysToWin t d = length $ filter (>d) $ zipWith (*) [0..t] (reverse [0..t])

waysToWin' :: Int -> Int -> Int
waysToWin' t d = go $ length $ fst $ break (>d) $ zipWith (*) [0..t] (reverse [0..t])
    where go n = t - n - n + 1

type Input = [(Int, Int)]
readInput :: String -> Input
readInput s = 
    let (times:distances:_) = lines s 
     in zip (map read $ tail $ words times) (map read $ tail $  words distances)
