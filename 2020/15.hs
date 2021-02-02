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
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    let input = [0,6,1,7,2,19,20]
    print $ part1 input
    print $ part2 input

part1 = solve 2020
part2 = solve 30000000

solve :: Int -> [Int] -> Int
solve n ls = go (length ls) (last ls) (IntMap.fromList $ init $ zip ls [0..])
    where
    go i prior _ | i >= n = prior
    go i prior stack = 
        let nxt = case IntMap.lookup prior stack of
                Nothing -> 0
                Just i' -> i-i'-1
         in go (i+1) nxt (IntMap.insert prior (i-1) stack)

turns :: [Int] -> [Int]
turns ls = ls ++ go (length ls) (last ls) (IntMap.fromList $ init $ zip ls [0..])
    where
    go i prior stack = 
        let nxt = case IntMap.lookup prior stack of
                Nothing -> 0
                Just i' -> i-i'-1
         in nxt : go (i+1) nxt (IntMap.insert prior (i-1) stack)

{-
-- Initial solution:

solve :: Int -> [Int] -> Int
solve n ls = go (length ls) (reverse ls)
    where
    go i stack
        | i >= n = head stack 
        | otherwise = go (i+1) $ let nxt = turnspeak stack in nxt : stack
    turnspeak (a:as) = case Data.List.findIndex (==a) as of
        Nothing -> 0
        Just i -> i+1

-}