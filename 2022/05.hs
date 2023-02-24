{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Lens
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
    input <- readFile "05.txt" :: IO String
    let game = parseInput input
    print $ part1 game
    print $ part2 game

part1 :: (Board, [Move]) -> String
part1 (stacks, moves) = map head $ elems $ foldl step stacks moves

step :: Board -> Move -> Board
step ss (n, from, to) = ss // [(from, drop n (ss ! from)), (to, reverse (take n (ss ! from)) ++ ss ! to)]

part2 :: (Board, [Move]) -> String
part2 (stacks, moves) = map head $ elems $ foldl step' stacks moves

step' :: Board -> Move -> Board
step' ss (n, from, to) = ss // [(from, drop n (ss ! from)), (to, take n (ss ! from) ++ ss ! to)]

type Board = Array Int Stack
type Stack = String
type Move = (Int, Int, Int)
parseInput :: String -> (Board, [Move])
parseInput s = (stacks, moves)
    where
    (slines, mlines) = break null $ lines s
    moves = map parseMove $ filter (not . null) mlines
    stacks = parseStacks slines

parseMove :: String -> Move
parseMove s = (read $ w !! 1, read $ w !! 3, read $ w !! 5)
    where w = words s

parseStacks :: [String] -> Board
parseStacks s = listArray (a,b) stacks
    where 
    vs = map V.fromList $ init s
    ns = words $ last s
    a = read $ head ns
    b = read $ last ns
    is = V.toList $ V.findIndices (/= ' ') $ V.fromList $ last s
    stacks = map (\i -> dropWhile (== ' ')  $ map (V.! i) vs) is