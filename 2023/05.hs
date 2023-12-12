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
    input <- readFile "05.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 (seeds, ranges) = go ranges seeds
    where
    go [] = minimum
    go (ranges:rs) = go rs . map (rangeFunction ranges)
part2 (seeds, ranges) = go ranges $ mkRanges seeds
    where
    go [] = minimum . map fst
    go (ranges:rs) = go rs . foldMap (rangeFunction' ranges)

rangeFunction :: [(Int, Int, Int)] -> Int -> Int
rangeFunction ranges = go
    where
    go n = case find (\(_, source, r) -> source <= n && n <= source + r - 1) ranges of
        Just (target, source, _) -> target + (n - source)
        Nothing -> n


mkRanges :: [Int] -> [(Int, Int)]
mkRanges (a:b:xs) = (a, a+b-1) : mkRanges xs
mkRanges _ = []

rangeFunction' :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
rangeFunction' ranges = go
    where
    go (low, hi) = case find (\(_, source, r) -> source <= hi && low <= source + r - 1) ranges of
        Just (target, source, r) -> 
            if source <= low 
                then if hi <= source + r - 1 
                        then [(target + (low - source), target + (hi - source))]
                        else (target + (low - source), target + r - 1) : go (source + r, hi)
                else go (low, source - 1) 
                  ++ if hi <= source + r - 1 
                        then [(target, target + (hi - source))]
                        else (target, target + r - 1) : go (source + r, hi)
        Nothing -> [(low, hi)]

type Input = ([Int], [Line])
parseInput :: P.Parser Input
parseInput = do
    "seeds: "
    seeds <- sepBy1' decimal (char ' ')
    endOfLine
    endOfLine

    ls <- sepBy1' parseLine endOfLine 
    skipSpace 
    endOfInput
    return (seeds, ls)

type Line = [(Int, Int, Int)]

parseLine :: P.Parser Line
parseLine = do
    skipSpace
    P.skipWhile (not . isEndOfLine) >> endOfLine -- skip first line
    ranges <- sepBy1' pl endOfLine
    endOfLine
    return ranges
    where
    pl = do
        skipSpace
        n <- decimal
        " "
        a <- decimal
        " "
        b <- decimal
        return (n, a, b)