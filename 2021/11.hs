{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, count)

main :: IO ()
main = do
    input <- getInput <$> readFile "11.txt" :: IO Board
    print $ part1 input
    print $ part2 input

part1, part2 :: Board -> Int
part1 b = sum $ map fst $ take 101 $ iterate (step . snd) (0,b)
part2 b = fromJust $ Data.List.findIndex ((==lim) . fst) $ iterate (step . snd) (0,b)
    where lim = length $ assocs b

step :: Board -> (Int, Board)
step = fmap simmerDown . glowUp . amap succ

simmerDown b = b // map (,0) (glowing b)

glowUp :: Board -> (Int, Board)
glowUp b = go (Set.fromList $ glowing b) mempty b 
    where
    go new old b | null new = (length old, b) 
    go new old b = 
        let ix = foldMap (neighbors b) new
            b' = accum (+) b (map (,1) ix)
            old' = new <> old
            new' = Set.fromList (glowing b') Set.\\ old'
         in go new' old' b'

glowing = map fst . filter ((>9).snd) . assocs

type Coord = (Int,Int)
type Board = UArray Coord Int

neighbors :: Board -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d)
    [ (x+dx,y+dy)
    | dx <- [-1..1]
    , dy <- [-1..1]
    , dx /= 0 || dy /= 0
    ]

getInput s = array ((0,0),(xmax, ymax))
    [ ((x,y),read [c])
    | (y,l) <- zip [0..] $ filter (not . null) $ lines s
    , (x,c) <- zip [0..] l
    ] 
    where
    ymax = pred $ length $ filter (not . null) $ lines s
    xmax = pred $ length $ head $ lines s