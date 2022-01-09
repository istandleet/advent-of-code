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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 input = 
    let ((x0,y0),(x1,y1)) = input 
        dy = pred $ abs $ min y0 y1
     in maximum $ map (snd . fst) $ arcy input (0,dy)

part2 :: Input -> Int
part2 input = 
    let ((x0,y0),(x1,y1)) = input
        mdy = pred $ abs $ min y0 y1
     in length $ filter (hits input) [(dx,dy) | dx <- [0..max x0 x1], dy <- [negate mdy - 1..mdy]]

type Input = (Coord, Coord)
type Coord = (Int,Int)
type Velocity = Coord

type St = (Coord, Velocity) -- pos vel 
step :: St -> St 
step ((x,y), (dx,dy)) = ((x+dx,y+dy),(dx - signum dx,dy-1)) 

input, example :: Input
input = ((56,-162),(76,-134))
example = ((20,-10),(30,-5)) -- target area: x=20..30, y=-10..-5

hits :: Input -> Velocity -> Bool
hits input v = fst v > 0 -- works for given inputs
            && any (inRange input . fst) (arc input v)

arc :: Input -> Velocity -> [St] 
arc ((x0,y0),(x1,y1)) v = takeWhile hittable $ iterate step ((0,0),v)
    where
    hittable ((x,y),(dx,dy)) = 
        (y >= min y0 y1 || dy > 0)
     && case compare dx 0 of 
            GT -> x <= max x0 x1
            EQ -> x >= min x0 x1 && x <= max x0 x1
            -- LT does not happen

arcy :: Input -> Velocity -> [St] 
arcy ((x0,y0),(x1,y1)) v = takeWhile hittable $ iterate step ((0,0),v)
    where hittable ((x,y),(dx,dy)) = y >= min y0 y1 || dy > 0
