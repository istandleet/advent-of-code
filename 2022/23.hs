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

main :: IO ()
main = do
    input <- readInput <$> readFile "23.txt"
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 = fin . (!!10) . going [U,D,L,R]
    where
    fin elves = (x1-x0+1)*(y1-y0+1) - length elves
        where
        x0 = minimum $ map fst $ Set.toList elves
        x1 = maximum $ map fst $ Set.toList elves
        y0 = minimum $ map snd $ Set.toList elves
        y1 = maximum $ map snd $ Set.toList elves

part2 :: Input -> Int
part2 = length . going [U,D,L,R]

going :: [Dir] -> Elves -> [Elves]
going dirs elves 
    | null neighboring = [elves]
    | otherwise = elves : going (rotate dirs) elves'
    where
    ns = foldMap neighbors elves
    neighboring = ns `Set.intersection` elves
    elves' = Set.difference elves ns <> step neighboring dirs 


type Input = Set Coord
type Elves = Input

type Coord = (Int, Int)
data Dir = U | D | L | R deriving (Show)

step :: Elves -> [Dir] -> Elves
step elves dirs = Set.map go elves
    where
    go c = maybe c (\c' -> if c' `elem` dupes then c else c') $ Map.lookup c m
    dupes = Set.fromList $ duplicates $ Map.elems m
    m = Map.fromSet (makemove' elves dirs) elves
    
makemove' :: Set Coord -> [Dir] -> Coord -> Coord
makemove' elves dirs c = go dirs
    where
    go [] = c
    go (d:ds)
        | all (`Set.notMember` elves) (side c d) = move d c
        | otherwise = go ds

makemove :: Set Coord -> [Dir] -> Coord -> Coord
makemove elves dirs c
    | all (`Set.notMember` elves) (neighbors c) = c
    | otherwise = go dirs
    where
    go [] = c
    go (d:ds)
        | all (`Set.notMember` elves) (side c d) = move d c
        | otherwise = go ds

duplicates :: Ord a => [a] -> [a]
duplicates = go mempty
    where
    go _ [] = []
    go s (x:xs)
        | x `Set.member` s = x : go s xs
        | otherwise = go (Set.insert x s) xs

move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ pred
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ succ

side :: Coord -> Dir -> [Coord]
side (x,y) = \case
    U -> [(x-1,y-1), (x,y-1), (x+1,y-1)]
    L -> [(x-1,y-1), (x-1,y), (x-1,y+1)]
    R -> [(x+1,y-1), (x+1,y), (x+1,y+1)]
    D -> [(x-1,y+1), (x,y+1), (x+1,y+1)]

neighbors :: Coord -> Set Coord
neighbors (x,y) = Set.fromDistinctAscList
    [ (x+dx,y+dy)
    | dx <- [-1..1]
    , dy <- [-1..1] 
    , dx /= 0 || dy /= 0
    ]

rotate [] = []
rotate (x:xs) = xs ++ [x]

-- * Parsing
readInput :: String -> Input
readInput s = Set.fromList
    [ (x, y)
    | (y, l) <- zip [0..] $ takeWhile (not.null) $ lines s
    , (x, c) <- zip [0..] l
    , c == '#'
    ]

-- * Drawing
draw :: Set Coord -> String
draw board = unlines
    [[tc $ (x,y) `Set.member` board | x <- [xmin..xmax]]
    | y <- [ymin..ymax]
    ]
    where
    xmin = minimum $ Set.map fst board
    xmax = maximum $ Set.map fst board
    ymin = minimum $ Set.map snd board
    ymax = maximum $ Set.map snd board
    tc b = if b then '#' else '.'
