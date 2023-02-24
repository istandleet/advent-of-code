{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
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
    input <- readFile "10.txt" :: IO String
    let ls = map parseLine $ filter (not.null) $ lines input
    print $ part1 ls
    putStrLn $ part2 ls

part1 :: [Instruction] -> Int
part1 = go . compute 1
    where
    go ls = sum $ map (\i -> i * ls !! pred i) [20,60..220]


part2 :: [Instruction] -> String
part2 = go . compute 1
    where
    go is = unlines 
        [ [if n <= i && i <= n+2 then '#' else '.' | (i,n) <- zip [1..] l]
        | l <- splitInto 40 is
        ]

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = let (a,b) = splitAt n xs in a : splitInto n b

compute :: Int -> [Instruction] -> [Int]
compute _ [] = []
compute n (i:is) = n : case i of
    Noop -> compute n is
    Addx x -> n : compute (n+x) is

data Instruction = Noop | Addx Int deriving (Show, Eq, Ord)

parseLine :: String -> Instruction
parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':xs) = Addx $ read xs
parseLine s = error $ "parseLine: " ++ s