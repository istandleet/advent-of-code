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
    input <- readFile "08.txt" :: IO String
    print $ part1 input
    print $ part2 input

type Coord = (Int, Int)
type Board = [[(Coord, Int)]]
type Board' = Array Coord Int

part1 :: String -> Int
part1 input = countVisibleTreesInGrid (parseInput input)

findVisibleTrees :: [(Coord,Int)] -> [Coord]
findVisibleTrees = go (-1)
    where 
    go !i [] = []
    go !i ((c,j):xs) = if j > i then c : go j xs else go i xs

countVisibleTreesInGrid :: Board -> Int
countVisibleTreesInGrid grid =
  let rows = grid
      cols = Data.List.transpose grid
  in length $ foldMap (Set.fromList . findVisibleTrees) (rows <> cols <> map reverse rows <> map reverse cols)

part2 :: String -> Int
part2 = part2' . buildBoard . mconcat . parseInput
     
buildBoard :: [(Coord,Int)] -> Board'
buildBoard ls = array (minimum $ map fst ls, maximum $ map fst ls) ls

part2' :: Board' -> Int
part2' b = maximum $ map (scenicScore b) $ indices b

scenicScore :: Board' -> Coord -> Int
scenicScore b = getscores
    where
    ((x0,y0),(x1,y1)) = bounds b
    getlines (x,y) = 
        [ [(x,y') | y' <- [y+1..y1]]
        , [(x,y') | y' <- [y-1,y-2..y0]]
        , [(x',y) | x' <- [x+1..x1]]
        , [(x',y) | x' <- [x-1,x-2..x0]]
        ]
    getscores c = product $ map (go 0 c) $ getlines c
    go !i _ [] = i
    go !i c (c':cs) = let i' = succ i in if b!c' >= b!c then i' else go i' c cs


parseInput :: String -> Board
parseInput input = addCoords $ map (map read . map pure) $ filter (not. null) $ lines input

addCoords :: [[a]] -> [[(Coord, a)]]
addCoords = zipWith (\ y -> zipWith (\ x -> (,) (x, y)) [0..]) [0..]

