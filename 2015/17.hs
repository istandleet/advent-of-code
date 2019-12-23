{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Function
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts(the,groupWith)
import Math.Combinat.Partitions

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "17"
    input <- getInput
    print $ part1 150 input
    print $ part2 150 input

getInput :: IO [Int]
getInput = map read . lines <$> readFile "17.txt" 

part1 :: Int -> [Int] -> Int
part1 target = go target . reverse . Data.List.sort
    where
    go _ [] = 0
    go n (l:ls)
        | l > n  = go n $ dropWhile (>n) ls
        | l == n = 1 + go n ls
        | otherwise = go (n-l) ls + go n ls
        
        
part2 :: Int -> [Int] -> Int
part2 target = go . part2' target
    where
    go ls = let i = minimum ls in length $ filter (==i) ls

part2' :: Int -> [Int] -> [Int]
part2' target = go target . reverse . Data.List.sort
    where
    go _ [] = []
    go n (l:ls)
        | l > n  = go n $ dropWhile (>n) ls
        | l == n = [1] ++ go n ls
        | otherwise = map succ (go (n-l) ls) ++ go n ls