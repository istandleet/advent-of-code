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
import Data.Functor
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
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 input = maximum $ map snd $ Set.toList $ snd $ foldl (uncurry turn) (cycle input, initBoard) (take 2022 $ cycle shapes)
part2 = const 3

type Dir = Bool -- True = Right, False = Left
type Input = [Dir]

type Coord = (Int,Int)
type Board = Set Coord
initBoard :: Board
initBoard = Set.fromList [(x,0) | x <- [0..6]]

type Shape = Set Coord

turn :: Input -> Board -> Shape -> (Input, Board)
turn input b s = 
    let s' = mkShape b s
     in go s' input 
    where
    go s (d:ds) = 
        case step b d s of
            Left b' -> (ds, b')
            Right s' -> go s' ds

step :: Board -> Dir -> Shape -> Either Board Shape
step b d = down . jet b d
    where 
    down s = 
        let s' = Set.map (second pred) s
         in if Set.disjoint s' b then Right s' else Left (b <> s)

jet :: Board -> Dir -> Shape -> Shape
jet b d s = 
    let s' = Set.map (first $ if d then succ else pred) s
     in if not $ all (inRange (0,6) . fst) s' && Set.disjoint b s' then s else s'

mkShape :: Board -> Shape -> Shape
mkShape s = Set.map (second (\y -> y + m + 4))
    where m = if null s then 0 else maximum (Set.map snd s)

shapes :: [Shape]
shapes = map Set.fromList
    [ [(x,0) | x <- [2..5]]
    , [(3,0),(2,1),(3,1),(4,1),(3,2)]
    , [(2,0),(3,0),(4,0),(4,1),(4,2)]
    , [(2,y) | y <- [0..3]]
    , [(2,0),(3,0),(2,1),(3,1)]
    ]

draw :: Board -> String
draw b = unlines $ 
    [ "|" ++ [if (x,y) `Set.member` b then '#' else '.' | x <- [0..6]] ++ "|"
    | y <- reverse [1..maxy]
    ] ++ ["+-------+"]
    where
    maxy = maximum $ Set.map snd b

input = map (=='>') "><<<<>><<><<<<>>>><<><<<>>><<>>>><<<<>>><<<><<<<>>><<<><<<>>>><<<><<<<><>>><>>><<<<><<<>>>><<>>>><<>><<>>><<<<>>><<>><>>><<>>>><<>><<<<><>><<>>>><<<><<<<><<<><><<>>><>><<<>>>><<<><<<<>>>><><<<<>><<<><>><>>><<>>><<<<>>>><<<<><<<<><<<><<><<<<>>>><>><<<<>><<>>><<<>>><><<<>>>><<<>>>><<<>><>><<>><<<>>>><<<<>>>><<<<>><><<<<>>>><><<>>>><<>><<<><<<>><><>>>><>><><<<<>>><<><>><<<>>>><><<><<><>>><<<<>>><>>><<<>>><<<<>><<>>>><>>>><>>>><<<<>>>><<<>>><>>><>>>><<>><<>>>><<<>>><<>>><<>>>><<<<>><<>>><>>><><<>>>><<><>>>><<>>><>><<<>><<>><><<<>>><<<>>>><><>>><<>>><<>>><<<<>>>><>>><<<<>>>><>><<<<>><<<<>>><<>><<>><>>><<<>><<<<>>><<>>>><<>><<<>>><><<>><<><<>><<<<>>>><<<<><>>>><><>>>><<<><><<<><<<<><<><>>>><><<<><<<<>>><>>>><<<<>><>>>><<>><<><><>><<<>>>><<>><<<>>>><<<<>>>><<<><<<>>><<<>><>><<<><<<>><<<<><<<>>><<<><>>><<>>><<<>>>><>><<>>>><<><<<>>><<<>>>><<<<>><<<>><<>><<<><>>><<>>><>>><<>><<>><<<<>>><>>><><<<<><<<<><<>>>><>>>><<>>>><<<<><<<<>><>><<<>>>><<<<>>><<>>>><<<<>>><<>>><<<<><<>>>><><><>>>><<>>>><<><<<>><<<>><<>>>><<>><<><<<<>>>><>><<<>><<<<><<<<>>><<>>><>>><<<<>>>><>><<<>>>><<<<>><>>>><<<>>>><>>><<<<>><<<<>>>><<<<>>><<<>>>><>><>>><<<><><<<>><<<<>>><>><<<>><>>>><<<>>>><<<>>><<<<>>><<<><<<>>><<>>>><<<>>><<<<>><<>>>><<<>><>>>><<>>>><<<<><<<><<>>>><><<<<>>>><<<<><<<<>>><<<>>>><<<<>>>><>><<<<>><<<<>>><><<<>><<<>>><>>>><<<>>><<><><<<<>>><<><<<>><<>>>><<<>><<<<>>><><<<>><<<>>>><<>>><<<>><<<<>>>><>>>><>>>><<<<><<<<>>><<><<>>>><<<<>><><>><<>><<<<>><<<>>>><>>>><<>><<>>><>>>><<<>>>><<<<>><<<>>>><<<<>><<>><<<<>>><<>><>>>><><<<<>>>><<>><<><<><<<>><>>>><<<>>>><<<>>><<<<><<<><>>><<>>><<>><<<>>><>><<<>>><<<>>>><>>><<>>>><<<>>><<><<>>><<>>>><>><<<><<><<<<>>><<<<>><>><<<>>><>><<<<>><<<<>><<>><<<<>>><<<<>>>><<<>>><<<<>>>><<><<>><<<><<<>>>><<<<><<<<>>><>>>><>><<<>>><<><><<>><<<>>>><<<><>>><<<>>><>>>><<><<<>><<<><<<<>><>>><<<>>>><>><<<><><<<>><<>><<<><<<>><<<<>>>><<<<>>><<>>><<<>>><<><<>><<<>>><<>>><<>><<<><>>><>>><<><<><><<>><>><<<><<<<>>><<<>>>><<<<>>>><>><<>>><>><>>>><<<>><<<<>>>><<<<>>><>>>><<<><<<<><<<><<<>>><<>>>><<<>><<<<>>>><<<<>>>><<<><<<<>><>>>><>><<>>>><<<<><<<>>><>>><<>>>><<<><<<<>>><<>><>><<>>><<<>>>><<<>>>><<<>>>><<<<>>><<<<>><><<<><<<>>>><<<>>>><<>><>>><<<><<<>><<<<><<><<><<<<>>><<>>>><<<<>>>><<>>><<<<>>>><<<<>>>><<<<><>><<<>><<><>>>><<<<><<>>>><<><<<>><<<<><<<<>><<>>>><<<>>><<>>>><>><<<<>><<<>><>>><<<<>>>><>>>><<<>><<<<><<>>>><<><>>>><><<>>><<<><<>><<<<><>>>><<<<><><>>>><<<<>>>><>>>><>><<<><>>><><<><<>>><<>>>><<<>>>><<><<<<><<<<><><<<<>>><<<>><<<><<<<>>><>>>><>>><<<><><>>>><>>>><<<>><<<>>>><<<<>>>><<<<><<<<>>>><<<><<><>><>>><>>>><<><<>>>><<<>>><<>>>><<<<>>><<<>>><<<>>><<<>><<>><<><<>>><<>>><<<>>><<<<>>>><>>>><<<>>>><>><>><><<>>><<<>><<<<>>><>><>><<<<>>><>><<>>><<<<>>><>>><>><<<>>>><<<<>>><<>>><>>>><>>>><<><<<<>>><<>><><<<<>><<<<>><<<<>>>><<<<>>>><<<<>>><<<>><<<>>>><<<<>>><>>><>>><>><<<>>><<><<>>><<>>><<<>>><<<<>>><<<>>>><<<>><<>>>><>><<<<>>>><<><<<<>>><<>><<>><>>>><<<>>>><<<>>>><>><<<<><>>><>><<>>><<<>>><<<>><<<>>>><<>>><<<>><<>>>><<<<>><<><<<<>>>><<>>><>>>><<<<><<<<><<><<<>><><<<>>><<<><<<><<<>>><<<<>><<<<>>><<<<>>>><>>><>>>><<<>>><<<<>><<<>>>><<<<>>><<<>><><<>>><<<>>><>>><<>>><<<>><<><<<>>><<>>><<>><<<<><<<>><<<<>>>><<<<>><>>><<<<>><<<><>>>><<>>>><>>><>><<>>>><<<>>>><<>>>><<<<>>><>>><>>><<<<>>>><>>>><>><<>><>><<<>><>><<>>><>>>><<<<>>><>>><<<><<<>>>><><<<>><>>><<<<><<<<><<<<><><<><<><<<><>>>><>><<>>><<<<>>><><<<<>><<<<><<<>>><<<>>>><<>><<<<><><<<>>>><>>><<<<><<<<>>>><>><><<<<>>>><<<<>>><>>><<<>><><<<><<<<>>>><<<<><<<>>><>><<<<>><<<>>>><<>>><<<><>><<>>>><<>>><>>><>>><<>>>><<<<>><>>>><>><<<<>>>><<>><<<>>>><<<<>>>><<>>>><<<>><<<<><<>>><><>>><>><<<<>>><<>><<<>>><<><<<>><<<<>><>>><<<><<>>><<>><<<>>>><<<>><<><<>>><<<><<<<><<<>>><<<><<><>>>><>>>><<<>>>><>><<<><<>>>><><<><<>>>><<<<>><<>>><<<<>>>><>>>><<>><<>><>>><<>><>>>><<<<>><>>><>>><<><>>>><<>><>>><<><><<<>>><>>>><<<>><<<<>>>><<<><<>>><<<<>>>><><<>>><<>><<<>>><<<>>>><>><<<<>>><<>>><>>>><>><<>>>><>>><>>><<>>><>><<<><<><<<<><>>>><<<>>>><>>>><<<<>>>><<<<><<<><<<>>><<<>><<>><<>>><<<>>><<<<>>>><<<>>><<<>>><<<><><<<>>>><>>>><>><<<>><<<>>>><><>>><<<>>>><>>>><<<>><<<<><>>><<<><<<<><<<>>>><<<<>>>><<<>>>><<><<>><>><<<>>><<<>><<<>>>><<<<>>><<<<>>><<<<><<<><<<><<<<>><<><<>>>><<<>>>><><<>><<>>><><<<<>>><<<><<<><<<>>>><<<>>><<<<>>>><<<<>><<<<>>>><><<>>><<><<>>><<><<<<><<<><><<<<><<>>>><>>><<<<>><<<<><<><<<<>>>><<<<>><<<>>>><>>><<<<>>><<<>>>><>>>><<<<>>><<<<>>>><>>><<<<>>><<<><><>>>><>>><<<<>>>><<<<><>><>>><><>><<>>><>>>><<<<><<<>>><<<<>>>><<<<>>>><<<>>><<<>>>><<<>>><<<<>><<><<>>><<<><<><<<<>>><<<<><<>><><<<><>>>><<<<>>>><<<<>>><<<>>><>>>><>><<<><<>>>><<<><<<<>>><<<><<<<>>><<<<>><><<>><<>>><>>><>>><<>>>><<>>>><<>>>><<<<><<<><<<<>>>><<>>>><><<<>><<<<>><<<>><<<>>><>><<>><<<<>>>><<<<>><><<>>>><<<>><<<<>>>><<<><<>>>><<<<>><<>>>><<<<>>>><>>>><>><>>><<<>><<<<>>>><<<>>><<<<><<<<><<><<>><<>><<<><<<<>><<<<><<>>>><<<<><>>>><>>>><>>><<>>>><<>>>><<<>>><<<><<>>>><<<><<<<>><<>>>><<><><<<<><<<><<>>>><<<>>><<<<>><<><<<>><>><<<<><><<<>>>><>><<>>>><<<>>>><<>>>><<<>><>><<>>><<<>>>><<<>>>><<<<>><<>>>><>><<<<>>>><<>><<<<>>><<>>><<><<<>>>><<<<>>><>><<>><<<<>><<<>><>><<>>>><<<>><<>>><<>><<<<>><><<<><<<<>>><<><<<><><<>>><<>><<<<>>><<<>><>><<<<>><<<<><<<<>>>><<<<>>><<>>><>>>><<<<>>><<<><<<<>><>>>><<<>>>><<><>>><<<>>><<<<>>>><<><<<>>>><<>>><<>><<<>><<<>>><<<><><><>><>>>><><>><<<>>>><<<>><<<>>>><<>><<>>><<<>>><<<>><<<<><<<<>>><>><<>><<<<>>>><<>>><<<<>><<<>>>><<<<>><<<<><>>><>>>><<>><<><<<>><<<><<<>><>>>><<<<>><<>>><<>>><<<>>>><>>>><<<<>>><<<><<<<>>><<>>><>><<>><<<>>>><<<<><>><>>><<<<>>>><>>>><<<><>>><<<><<<<>>><<<><<><<<<>><<>><<>>>><<<>>>><<<<>>>><<>><<<<><<<>>><><<<><<<>>><<<>>><><<<>>><<>>><<<<>><<<<>><<>>>><<><<<<>><<<><>>>><<<>>><<<>><<>>>><<<<><<<<>>>><<>>>><>>>><<<>>><<>>>><<<>>>><>>><<<<>>><>>>><>><><<<<>>>><<<>>><><<<><<<>>>><><<>>><<<<>>>><>><<><><<<>>><<<><<<<>>><<>><<<>>>><<<>>><<<<>><<<>><<<><<>>><<<<>>><><<>>><<<>>>><<<<>>>><<<<><<<>><>>>><>>>><<>>><<<>><<<>>>><<>><<>>>><<<><<>><><<><<><<><<<>>><>>>><>><><<<<>><>><>>>><>>><<<<>>>><<<<>>>><>>><<<>><<<<>>><<>>><<<<><<<<>>>><<<<>>>><<><<<>><<>>><<>>><<<>>>><<<<>>>><<<<>>><<>>><<><<<<><<>>>><<><<<<>><<<<>>><<<<>><<<>><<><>>><<>>>><<<<>>><<<<>>>><<<>>><>>><<<<>>>><<<<>>><<><>>><<><<<>><>>>><>>><<<<><<>><<>>>><>>><<<>>>><><<<<>>>><<<<>><<<>><<<><<<<>>><<<<>>><<>><<><<<<>>><><<>>>><<<>><<>><<<<>><<>><<>><<>>><<<<>><<<><<>>><><>>>><<<>>><<<><><<><<<<>>>><<<<>>>><><<<<><>>><<<>>><<<>><<>>>><<>><<<<>>><<><<<<>>>><<<>>>><<<>><<<>>>><>><<>><>><<<>><<><<><<<><<<<>>><>><<<<>><<<>>><<<<>>><<<<>><<<<>><<<<>>><<<<><>><<<><>>>><<>>>><<<<>><<<<>>><<>><>>><>>>><<<>>><>><<<>>><<><<<>>><<>><>>>><>><>><<><>>>><<<>>>><<>>>><<<>>>><<<>>><<<>>><<><<>>>><>>><>><<>>><<<>>><<>><<><<>><<>><<>>>><<>>>><>>><<<>>>><><<<<>><<<>>>><<<><<<<>>><<<<>>>><<>><<<><<<<>>>><><<>>>><>>><<<>>>><<<>>>><<><>><<<>>><<<<>>>><<<<><>>><<<<>><<<<>>><<<<><>>><<<<>><>>>><<<<>>><<>>>><<>>><<<<>>>><>>><><<>>>><<<><>><<<<>>>><<<<>>><<<>>>><<>><<<<>>>><>><<<><>><>>><>>><<>>><<<<>><<<>>>><<<>>><<<>>>><<<<>>><<<<>><<<<>>>><<<>>><>>>><<<>>><<<<><<<<><<>><<<<><<<<>><>><<<<>>>><<>><>>><<<><<>>><<<><<<<>><<<>>>><>>><>><<<>><<<<>>>><<<>>>><<>><>>>><<<<><<>>><<><<>>><>>><<>>><<<<>>><<<<>>><<<<>>><>>><>>>><<<><<<<>><<<>><<>>>><<<<>><<<><<<><>>>><<<>>>><<>>><<>>>><<<><<><<<><<<<>><>>>><<<<>><<><<>>>><<<<>><<<<>>><<<<>><>><<<<>>><>>>><<<<>>>><<>>>><<>>>><>>><<>><>>>><<<<>>><<<<>><>>><<>><<<<><<<>><><<<<>>>><<<<>><>>><<<<>><>>>><<><><<<<>><<<<>>><<<<><><<<>>><<><<<>><<<>>>><>>><>>>><<>>>><<>><<<>>>><>>>><<><<>>>><<><<<<>>>><>>>><<>>><<>>><><<<<>>><<<<>>><<<>><<>><>>>><<><>>><>>><<<<>><<<<>>>><<<>>>><<<<>>>><<<<>>>><<<>><>>>><<<>><<>><>>><>>><<><<<<>>>><<>><<<>>>><<<<><<<<>>>><<<><<<><<<<><<<>>><<<<>>>><<>>>><>><<<<><<>><<<<>>>><<<>>><>>>><<>>>><<<<>>><><>>>><<><>>>><<><<<<><<<><>>>><<><<<>>>><>><<<>>>><<<><>><>>>><<<>>><>>>><<>>>><>>>><<<<><>>>><<><<>><<<<><>>><<<<>>>><<<<><<<<><><<><<<>>><<<<>>>><<<<><<<>><<<<><>><>><<<>><<<<>><<>><<<><<<><<>>><<<<><<>><<<<>>><<>>>><<<<>><<>>>><<<<>>>><<<<>>><<<<><<<>>>><<>>>><<<<>><>>>><<<<><><<><>><<<<><>>>><<<>>><<<>><<<>><<<<>>><<<<>>>><>>>><><<<<>>>><>><>>>><<<>>><<<<><>>>><<>>><<<<><<<>><<<>>><<>>><<<<>>>><<<>>><<<>>>><<>><<>>><<><<<>>><<<<>>><<<>>><<<<>><><>>><<<>><<<<>><<>><><<<<>>><>>>><<<><<<<><<><<<>>><<<>><>>><<<<><<>><<<><<<>>><>><<<<>><>>>><<<>>>><<<><><<>>><>><<>><<<<>><>>>><><<<>>>><<<>><<>>>><<>>>><<>><<<<>><><>>>><<>>>><<<<><>><<><<<<>><<>>>><<<>>><<<>><<<<>><<>>>><>>>><<>>>><<<>>>><<<<><<><<<>>>><<>>>><<<>>><>>>><>><>>>><<<<>><>><><>><<><<<>><<><<<>>><<<<><<<<><<<<>>><<>>>><<<>>><<><>>>><<>>>><<>>><<>>>><<><<<<>><>>>><<<>><<<>>><<<>>>><<>><>>><>><<<>><<<>><<>>><<<><<>><<<<><<<>>>><>>><<<><<<>>>><<<>><>><>>>><>><><<>><<<<>>><<<>><<>><<<>>>><<<<>><<<>><>>>><<<<>>>><>><>><<<<>><<<>>>><<>>><<<<><>>>><<<>>>><<<<><<>>>><<><>><<>>><<<<>><<<<>>><<<<>>>><>>>><<>><<<<>>>><<>>>><<>>><<>>>><<<><<<>>><<>>><<<>><<<>>><<><<<><<<<>>>><<<><<><>>>><<>>><<>>><><<>>><<<<><<<<>>><>><<>>>><<<<>>><<<<><>>>><<<<>>>><>>>><<<>>>><<<<>>>><<<<>>><<<<>><>>><>><<><<<<>>>><>>>><>>>><<<>><>><<<><><<><<<<>><<>>><<<>><<<><<<<><<<>><<<<>><<<<><<>><<<>>><<<<><<>>><>><>>>><<>>>><<<<>>><><<<<>>><>><<<<><<<<>><<<>>><<<<><<><<><<>><<><<>>><>><<<<>>><<<>><<<<>><<<<><<<>>>><>>><><<<>><<><<>><<>>>><<>>><<>>>><<<<>>>><>>>><>>><<>>><<>>>><>>><<><<<<>>><<<>><<<<>><<<>>><<<<>>><<<><<>>>><<<>>>><<<<>><<><>>><<<><<<<>>>><><<>>>><<<>>>><><<>>>><<>>>><<<<>><<<<><<<><<>><>><>><>>><<><>>><<><<>><<<<><<>><<<<>><>><<<<>>><<<<>><<<<>>><<<><>>>><<<>>>><>>>><><>>>><<>><<<<>><>><<<>>>><<>>>><<>>><<<>>>><><>>>><<<>>>><<<<>><<<>><<>>><<><<<>><<<<>>>><<>>>><>>>><<><<>>>><>>>><<<<>><<<<><<>>><><<><<<>>>><><<>><<<>>><>>>><<>>>><><<><<>>><>><>><<<<><<>>>><<<<>><<<<>>><>>>><<<>>><<<<><>>>><<<<>>>><<>>><<>>><<<>><>><<<<><<<<>>>><><<<>><<<<>>>><<><<<>><<<>><<<<>>>><><<><>>>><><<<<>>><<<>>><<>>>><>>>><<<<>><<><<>>>><<<><<<><>>><>>><<>><<<>><><<<<>>><<<<><>>>><<<>><<>><<>><<<>><<<<>>>><<<>><>>>><<>><>><>>><<<>><>><<<<><<<<>>>><<<<>>><<<>>>><<<<>>><<<<>>><<<<>>><<><>>>><<>>>><>>><<>>>><<<>>><<>>><>><<<>>>><>><<>>><<><<>>><<<>>>><<><<><><<<>>><<<>>>><<<>><<<<>><<><<<<>>><>>>><<<<><<>>><<<>>>><<><<<>>><><<>>><<<<><<<>>><>><><<<>>><<<>>>"
inputx = map (=='>') ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

