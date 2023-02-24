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

import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take, I)

main :: IO ()
main = do
    input <- readFile "14.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1, part2 :: [[Coord]] -> Int
part1 = pred . length . iterateMaybe fill . buildOccupation . foldMap buildShape
part2 cs = pred . length . iterateMaybe (fill' floor) . buildOccupation . foldMap buildShape $ cs 
    where floor = maximum (map snd $ mconcat cs) + 2

type Occupation = Map Int (Set Int) -- ^ Map from x values to y values
fill :: Occupation -> Maybe Occupation
fill occ = go <$> dropSand occ (500, 0)
    where go (x,y) = Map.adjust (Set.insert y) x occ

dropSand :: Occupation -> Coord -> Maybe Coord
dropSand occ (x,y) = do
    ys <- Map.lookup x occ
    let (_above, below) = Set.split y ys
    y' <- Set.lookupMin below
    ys' <- Map.lookup (pred x) occ 
    if y' `Set.notMember` ys'
        then dropSand occ (pred x, y') 
        else do
            ys'' <- Map.lookup (succ x) occ
            if y' `Set.notMember` ys''
                then dropSand occ (succ x, y')
                else Just (x, pred y')

fill' :: Int -> Occupation -> Maybe Occupation
fill' floor occ = go $ dropSand' floor occ (500, 0)
    where 
    go (x,y) = case Map.lookup x occ of 
        Nothing -> Just $ Map.insert x (Set.singleton y) occ
        Just ys -> if y `Set.member` ys then Nothing else Just (Map.insert x (Set.insert y ys) occ)

dropSand' :: Int -> Occupation -> Coord -> Coord
dropSand' floor occ = \(x,y) -> case Map.lookup x occ >>= Set.lookupMin . snd . Set.split y of
    Nothing -> (x, pred floor)
    Just y' -> case Map.lookup (pred x) occ of
        Nothing -> dropSand' floor occ (pred x, y')
        Just ys' | y' `Set.notMember` ys' -> dropSand' floor occ (pred x, y')
        _ -> case Map.lookup (succ x) occ of
            Nothing -> dropSand' floor occ (succ x, y')
            Just ys'' | y' `Set.notMember` ys'' -> dropSand' floor occ (succ x, y')
            _ -> (x, pred y')

type Shape = Set Coord
buildShape :: [Coord] -> Shape
buildShape cs = foldMap Set.fromList $ zipWith intercalate cs (tail cs)

buildOccupation :: Shape -> Occupation
buildOccupation s = Map.fromList
    [ (the x, Set.fromList y)
    | (x, y) <- Set.toList s
    , then group by x using groupWith
    ]

intercalate :: Coord -> Coord -> [Coord]
intercalate (x,y) (x',y') 
    | x == x' = [(x,y) | y <- [min y y' .. max y y']]
    | y == y' = [(x,y) | x <- [min x x' .. max x x']]
    | otherwise = error "intercalate: not aligned"

type Coord = (Int, Int)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)

-- * Parsing
parseInput :: Parser [[Coord]] 
parseInput = sepBy1' parseLine endOfLine <* skipSpace <* endOfInput

parseLine :: Parser [Coord]
parseLine = sepBy1' parseCoord " -> "

parseCoord :: Parser Coord
parseCoord = (,) <$> decimal <* "," <*> decimal
