{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Data.Char
import Data.Foldable
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
    s <- readFile "01.txt"
    let input = map read $ lines s :: [Int]
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Maybe Int
part1 is = twosat (Set.fromList is) 2020

part2 :: [Int] -> [Int]
part2 is = 
    let sis = Set.fromList is
     in mapMaybe (\i -> fmap (*i) $ twosat (Set.delete i sis) (2020-i)) is

twosat :: Set Int -> Int -> Maybe Int
twosat is n = 
    let found = listToMaybe $ filter (\i -> (n-i) `Set.member` is) $ Set.toList is
     in fmap negate $ (*) <$> found <*> (subtract n <$> found)
