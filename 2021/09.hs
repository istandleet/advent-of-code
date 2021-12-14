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
    input <- getInput <$> readFile "09.txt" :: IO Board
    print $ part1 input
    print $ part2 input

part1, part2 :: Board -> Int
part1 b = sum $ map (+1) $ map (b!) $ lowPoints b
part2 b = product $ take 3 $ reverse $ Data.List.sort $ map length $ map (connectedTo  b) $ lowPoints b

lowPoints :: Board -> [Coord]
lowPoints b = 
    [ i
    | i <- range bnds
    , let x = b ! i 
    , all ((>x) . (b!)) $ neighbors b i
    ]
    where
    bnds = bounds b

connectedTo :: Board -> Coord -> Set Coord
connectedTo board ix = go (Set.singleton ix) (Set.singleton ix)
    where
    go new acc | null new = acc
    go new acc = 
        let new' = Set.filter ((/=9) . (board !)) 
                 $ foldMap (Set.fromList . neighbors board) 
                 $ new 
         in go (new' Set.\\ acc) (new' <> acc)

type Coord = (Int,Int)
type Board = UArray Coord Int

neighbors :: Board -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d)
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]

getInput s = array ((0,0),(xmax, ymax))
    [ ((x,y),read [c])
    | (y,l) <- zip [0..] $ filter (not . null) $ lines s
    , (x,c) <- zip [0..] l
    ] 
    where
    ymax = pred $ length $ filter (not . null) $ lines s
    xmax = pred $ length $ head $ lines s