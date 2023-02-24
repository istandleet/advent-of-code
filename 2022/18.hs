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
    input <- readFile "18.txt"
    let cubes = map readLine $ filter (not . null) $ lines input
    print $ part1 cubes
    print $ part2 cubes

part1, part2 :: Input -> Int
part1 cs = sum $ map area cs 
    where 
    s = Set.fromList cs
    area c = length $ filter (`Set.notMember` s) $ neighbors c
part2 cs = part1 (cs <> fill cs)

neighbors :: Cube -> [Cube]
neighbors (x,y,z) = 
    [ (x+1,y,z)
    , (x-1,y,z)
    , (x,y+1,z)
    , (x,y-1,z)
    , (x,y,z+1)
    , (x,y,z-1)
    ]

fill :: [Cube] -> [Cube]
fill cs = go wall $ range bnds
    where
    wall = Set.fromList cs
    go s [] = []
    go !s (ix:ixs)
        | ix `Set.member` s = go s ixs
        | otherwise = 
            let (b, brothers) = broaden bnds wall ix
             in (if b then (Set.toList brothers <>) else id) $ go (brothers <> s) ixs

    xmin = minimum $ map (\(x,_,_) -> x) cs
    xmax = maximum $ map (\(x,_,_) -> x) cs
    ymin = minimum $ map (\(_,y,_) -> y) cs
    ymax = maximum $ map (\(_,y,_) -> y) cs
    zmin = minimum $ map (\(_,_,z) -> z) cs
    zmax = maximum $ map (\(_,_,z) -> z) cs
    bnds = ((xmin,ymin,zmin),(xmax,ymax,zmax))

broaden :: (Cube, Cube) -> Set Cube -> Cube -> (Bool, Set Cube)
broaden bnds wall = go mempty . Set.singleton
    where
    go seen new 
        | null new' = (True, seen')
        | not $ all (inRange bnds) new' = (False, seen' <> new')
        | otherwise = go seen' new'
        where
        seen' = new <> seen
        new' = Set.filter (\ix -> ix `Set.notMember` seen && ix `Set.notMember` wall)
                $ foldMap (Set.fromList . neighbors) new

type Cube = (Int, Int, Int)
type Input = [Cube]

-- * Parsing
readLine :: String -> Cube
readLine s = case words $ map (\c -> if c == ',' then ' ' else c) s of
    [x,y,z] -> (read x, read y, read z)
    _ -> error "bad input"
