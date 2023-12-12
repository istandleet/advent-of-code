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
    input <- readInput <$> readFile "11.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 s = sum
    [ shortest a b 
    | (a:as) <- Data.List.tails $ Set.toList s
    , b <- as
    ]
    where
    (xs, ys) = empties s
    shortest (x0, y0) (x1, y1) = 
        let extrax = length $ fst $ Set.split (max x0 x1) $ snd $ Set.split (min x0 x1) xs
            extray = length $ fst $ Set.split (max y0 y1) $ snd $ Set.split (min y0 y1) ys
         in extrax + extray + abs (x0 - x1) + abs (y0 - y1)
part2 s = sum
    [ shortest a b 
    | (a:as) <- Data.List.tails $ Set.toList s
    , b <- as
    ]
    where
    (xs, ys) = empties s
    shortest (x0, y0) (x1, y1) = 
        let extrax = (*(1000000-1)) $ length $ fst $ Set.split (max x0 x1) $ snd $ Set.split (min x0 x1) xs
            extray = (*(1000000-1)) $ length $ fst $ Set.split (max y0 y1) $ snd $ Set.split (min y0 y1) ys
         in extrax + extray + abs (x0 - x1) + abs (y0 - y1)

empties :: Set Coord -> (Set Int, Set Int)
empties s = (xs, ys)
    where
    (x0, x1) = (\s -> (minimum s, maximum s)) $ Set.map fst s
    (y0, y1) = (\s -> (minimum s, maximum s)) $ Set.map snd s
    xs = Set.fromList [x0..x1] Set.\\ Set.map fst s
    ys = Set.fromList [y0..y1] Set.\\ Set.map snd s

type Coord = (Int, Int)
type Input = Set Coord
readInput :: String -> Input
readInput s = Set.fromList
    [ (x, y)
    | (y, l) <- zip [0..] $ lines s
    , (x, c) <- zip [0..] l
    , c == '#'
    ]
