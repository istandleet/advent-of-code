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
    input <- readFile "04.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 = sum . map worth
    where
    f 0 = 0
    f n = 2^(n-1)
    worth :: Line -> Int
    worth (_, winning, have) = 
        let winners = Set.fromList winning
         in f $ length $ filter (`Set.member` winners) have
        
part2 = fst . foldr (uncurry . go) (0, mempty)
    where
    go :: Line -> Int -> Map Int Int -> (Int, Map Int Int)
    go (i, a, b) score m =
        let winners = length $ filter (`Set.member` (Set.fromList a)) b
            newcards = sum $ map (m Map.!) [succ i..i+winners]
            score' = score + newcards + 1
         in (score', Map.insert i (newcards + 1) m)


type Input = [Line]
parseInput :: P.Parser Input
parseInput = sepBy1' parseLine endOfLine <* skipSpace <* endOfInput

type Line = (Int, [Int], [Int])

parseLines :: P.Parser [Line]
parseLines = parseLine `P.sepBy1'` "\n" <* P.skipSpace <* P.endOfInput

parseLine :: P.Parser Line
parseLine = do
    "Card"
    skipSpace
    n <- decimal
    ":"
    skipSpace
    a <- sepBy1' decimal skipSpace
    skipSpace
    "|"
    skipSpace
    b <- sepBy1' decimal skipSpace
    return (n, a, b)