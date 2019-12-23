{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Data.List
import Data.Array.Unboxed 

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Primes


main = do
    putStrLn "5"
    input <- map read . lines <$> readFile "5.txt"
    print $ part1 input
    print $ part2 input
    
part1 :: [Int] -> Int
part1 = go 0 0 . Vector.fromList 
    where
    go !n !i is | i < 0 || i >= length is = n
    go !n !i is = go (n+1) (i + is Vector.! i) (Vector.modify (change i) is)
    
    change i v = MVector.modify v succ i 
    
    
part2 :: [Int] -> Int
part2 = go 0 0 . Vector.fromList 
    where
    go !n !i is | i < 0 || i >= length is = n
    go !n !i is = go (n+1) (i + is Vector.! i) (Vector.modify (change i) is)
    
    change i v = MVector.modify v (\n -> if n >= 3 then n-1 else n+1)  i 