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
    input <- readFile "09.txt" :: IO String
    let ls = parseInput input
    print $ part1 ls
    print $ part2 ls

part1 :: [(Dir, Int)] -> Int
part1 = length . Set.fromList . map fst . scanl (flip step) ((0,0), (0,0)) . unfold

part2 :: [(Dir, Int)] -> Int
part2 = length . Set.fromList . map last . scanl (flip step') (replicate 10 (0,0)) . unfold

unfold :: [(a, Int)] -> [a]
unfold [] = []
unfold ((d,n):ls) = replicate n d ++ unfold ls

step :: Dir -> Board -> Board
step dir (t, h) = (t', h')
    where
    t' = if maxDist t h' > 1 then h else t
    h' = move dir h

step' :: Dir -> Board' -> Board'
step' dir (h:ts) = h':go h' ts
    where
    go _ [] = []
    go a (b:bs) = let b' = follow a b in b' : go b' bs
    h' = move dir h

follow :: Coord -> Coord -> Coord 
follow (x,y) (x',y')
    | abs dx `max` abs dy <= 1 = (x', y')
    | otherwise = (x' + signum dx, y' + signum dy)
    where 
    dx = x-x'
    dy = y-y'

maxDist :: Coord -> Coord -> Int
maxDist (x,y) (x',y') = abs (x-x') `max` abs (y-y')

type Board = (Coord, Coord)
type Board' = [Coord]
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)
move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ succ
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ pred

parseInput :: String -> [(Dir, Int)]
parseInput = map parseLine . filter (not . null) . lines

parseLine :: String -> (Dir, Int)
parseLine l = case words l of
    [d, n] -> (read d, read n)