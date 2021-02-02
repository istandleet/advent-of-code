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
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    s <- readFile "17.txt"
    print $ part1 $ readBoard3 s
    print $ part2 $ readBoard4 s

part1 :: Board3 -> Int
part1 = length . filter id . elems . (!!6) . iterate step3

type Board3 = UArray Coord3 Bool

step3 :: Board3 -> Board3
step3 = shrinkBoard3 . stepBoard3

stepBoard3 :: Board3 -> Board3
stepBoard3 arr = array bnds $ map (\ix -> (ix, go ix)) $ range bnds
    where
    ((x0,y0,z0),(x1,y1,z1)) = bounds arr
    bnds = ((x0-1,y0-1,z0-1),(x1+1,y1+1,z1+1))
    go ix = 
        let n = length $ filter id $ map (arr!) $ filter (inRange (bounds arr)) $ neighbors3 ix
         in if inRange (bounds arr) ix && (arr ! ix) then n == 2 || n == 3 else n == 3

shrinkBoard3 :: Board3 -> Board3
shrinkBoard3 arr = array bnds $ map (\ix -> (ix, arr ! ix)) $ range bnds
    where
    bnds = go $ map fst $ filter snd $ assocs arr
    go (ix:ixs) = foldl expand (ix,ix) ixs
    expand ((x0,y0,z0),(x1,y1,z1)) (x,y,z)
        = ((x0 `min` x,y0 `min` y,z0 `min` z),(x1 `max` x,y1 `max` y,z1 `max` z))

part2 :: Board4 -> Int
part2 = length . filter id . elems . (!!6) . iterate step4

type Board4 = UArray Coord4 Bool

step4 :: Board4 -> Board4
step4 = shrinkBoard4 . stepBoard4

stepBoard4 :: Board4 -> Board4
stepBoard4 arr = array bnds $ map (\ix -> (ix, go ix)) $ range bnds
    where
    ((x0,y0,z0,w0),(x1,y1,z1,w1)) = bounds arr
    bnds = ((x0-1,y0-1,z0-1,w0-1),(x1+1,y1+1,z1+1,w1+1))
    go ix = 
        let n = length $ filter id $ map (arr!) $ filter (inRange (bounds arr)) $ neighbors4 ix
         in if inRange (bounds arr) ix && (arr ! ix) then n == 2 || n == 3 else n == 3

shrinkBoard4 :: Board4 -> Board4
shrinkBoard4 arr = array bnds $ map (\ix -> (ix, arr ! ix)) $ range bnds
    where
    bnds = go $ map fst $ filter snd $ assocs arr
    go (ix:ixs) = foldl expand (ix,ix) ixs
    expand ((x0,y0,z0,w0),(x1,y1,z1,w1)) (x,y,z,w)
        = ((x0 `min` x,y0 `min` y,z0 `min` z,w0 `min` w)
          ,(x1 `max` x,y1 `max` y,z1 `max` z,w1 `max` w))

-- * Parsing
readBoard3 :: String -> Board3
readBoard3 s = array bnds
    [ ((x,y,0), c == '#')
    | (y,l) <- zip [0..] ls
    , (x,c) <- zip [0..] l
    ]
    where
    ls = takeWhile (not.null) $ lines s
    bnds = ((0,0,0),(pred $ length $ head ls, length ls - 1,0))

readBoard4 :: String -> Board4
readBoard4 s = array bnds
    [ ((x,y,0,0), c == '#')
    | (y,l) <- zip [0..] ls
    , (x,c) <- zip [0..] l
    ]
    where
    ls = takeWhile (not.null) $ lines s
    bnds = ((0,0,0,0),(pred $ length $ head ls, length ls - 1,0,0))

-- * Utils
drawBoard :: Board3 -> String
drawBoard arr = unlines $ map drawLevel [z0..z1]
    where
    drawLevel z = unlines $ ("z=" ++ show z) :
        [ [if arr!(x,y,z) then '#' else '.' | x <- [x0..x1]]
        | y <- [y0..y1]
        ]
    ((x0,y0,z0),(x1,y1,z1)) = bounds arr

type Coord3 = (Int,Int,Int)

neighbors3 :: Coord3 -> [Coord3]
neighbors3 ix = map (`adjust3` ix) cardinalities3

adjust3 :: (Int,Int,Int) -> Coord3 -> Coord3
adjust3 (dx,dy,dz) (x,y,z) = (x+dx, y+dy, z+dz)

cardinalities3 :: [(Int,Int,Int)]
cardinalities3 = tail $ (,,) <$> [0,-1,1] <*> [0,-1,1] <*> [0,-1,1]

type Coord4 = (Int,Int,Int,Int)

neighbors4 :: Coord4 -> [Coord4]
neighbors4 ix = map (`adjust4` ix) cardinalities4

adjust4 :: (Int,Int,Int,Int) -> Coord4 -> Coord4
adjust4 (dx,dy,dz,dw) (x,y,z,w) = (x+dx, y+dy, z+dz, w+dw)

cardinalities4 :: [(Int,Int,Int,Int)]
cardinalities4 = tail $ (,,,) <$> [0,-1,1] <*> [0,-1,1] <*> [0,-1,1] <*> [0,-1,1]
