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
    input <- readFile "15.txt"
    let cs = map readLine $ filter (not . null) $ lines input
    print $ part1 2000000 cs
    print $ part2 cs

part1 :: Int -> [(Coord, Coord)] -> Int
part1 y cs = totalSize (foldMap go cs) - length (filter (==y) $ map snd $ Set.toList $ Set.fromList $ map snd cs)
    where go (s, b) = planeFrom y s (manhattanDist s b)
-- part2 :: [(Coord, Coord)] -> Int
part2 cs = 
    [ (y, r) 
    | y <- [0..4000000]
    , let r = foldMap (\(s,b) -> planeFrom y s (manhattanDist s b)) cs
    , totalSize (restrict (0, 4000000) r) < 4000001 
    ]

planeFrom :: Int -> Coord -> Int -> Range
planeFrom y (x, y') dist = if d >= 0 then Range [(x-d, x+d)] else mempty
    where d = dist - abs (y'-y)

type Coord = (Int, Int)

newtype Range = Range {unRange :: [(Int, Int)]} deriving (Show, Eq, Ord)
instance Semigroup Range where Range xs <> Range ys = Range $ merge xs ys
instance Monoid    Range where mempty = Range mempty
totalSize :: Range -> Int
totalSize = sum . map (\(a,b) -> b-a+1) . unRange

restrict :: (Int, Int) -> Range -> Range
restrict (minimal, maximal) = Range . map go . unRange
    where
    go (a,b) = (max a minimal, min b maximal)

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge [] = id
merge (x:xs) = merge xs . go
    where
    go [] = [x]
    go (y:ys) = case mergeMaybe x y of
        Nothing -> y : go ys
        Just z -> merge [z] ys

mergeMaybe :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
mergeMaybe (a, b) (a', b') 
    | a > a' = mergeMaybe (a', b') (a, b)
    | b < a' - 1 = Nothing
    | otherwise = Just (min a a', max b b')

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')

-- * Parsing
readLine :: String -> (Coord, Coord)
readLine s = case words s of 
    [_,_,x1,y1,_,_,_,_,x2,y2] -> ((read $ drop 2 $ init x1, read $ drop 2 $ init y1), (read $ drop 2 $ init x2, read $ drop 2 y2))
    _ -> error $ "readLine: " ++ s
