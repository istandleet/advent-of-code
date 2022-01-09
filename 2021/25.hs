{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, count, Done)

import Data.Time

main = do
    putStrLn "25"
    input <- readBoard <$> readFile "25.txt"
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print

part1 :: Board -> Int
part1 b = succ $ ffe $ iterate step b 
    where ffe l = length $ takeWhile not $ zipWith (==) l (tail l)

type Coord = (Int, Int)

type Positions = Array Coord Bool 
type Board = (Positions, Positions)

step :: Board -> Board
step (a,b) = (a',b')
    where 
    a' = listArray (bounds a)
        [     (a!(x,y) && (a!(mox $ x+1, y) || b!(mox $ x+1, y)))
           || (a!(mox $ x-1,y) && not (a!(x,y) || b!(x,y))) 
        | (x,y) <- range $ bounds a
        ]
    b' = listArray (bounds a)
        [ (b!(x,moy $ y-1) && not (a'!(x,y) || b!(x,y))) || (b!(x,y) && (a'!(x,moy $ y+1) || b!(x, moy$ y+1)))
        | (x,y) <- range $ bounds a
        ]
    ((xmin,ymin),(xmax,ymax)) = bounds a
    mox = flip mod (xmax+1)
    moy = flip mod (ymax+1)

readBoard :: String -> Board
readBoard s = go $ Data.List.partition snd
    [ ((x,y), c == '>')
    | (y,l) <- zip [0..] ls
    , (x,c) <- zip [0..] l
    , c == 'v' || c == '>'
    ]
    where
    ls = takeWhile (not.null) $ lines s
    bnds = ((0,0),(pred $ length $ head ls, pred $ length ls))
    go (a,b) = (listArray bnds $ map (`Set.member` Set.fromList (map fst a)) $ range bnds
               ,listArray bnds $ map (`Set.member` Set.fromList (map fst b)) $ range bnds)

drawBoard :: Board  -> String
drawBoard (a,b) = init $ unlines 
    [   [ if a !(x,y) then '>' else if b!(x,y) then 'v' else '.'
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds a