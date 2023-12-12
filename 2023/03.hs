{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Lens

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Time
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
import Data.Attoparsec.Text as P hiding (takeWhile, take, D)

main = do
    input <- readInput <$> readFile "03.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 Input{..} = sum $ map snd $ filter (not . null . Set.intersection specs . fst) numbers
    where specs = foldMap (Set.fromList . neighbors . fst) symbols
part2 Input{..} = sum $ go $ map fst $ filter ((=='*'). snd) symbols
    where
    go [] = []
    go (c:cs) = 
        let neighbors' = Set.fromList $ neighbors c
            ns = map snd $ filter (not . null . Set.intersection neighbors' . fst) numbers
         in if length ns == 2 then product ns : go cs else go cs

type Coord = (Int, Int)
neighbors :: Coord -> [Coord]
neighbors (x,y) =
    [ (x+dx,y+dy)
    | dy <- [-1..1] 
    , dx <- [-1..1]
    ]

data Input = Input 
   { symbols :: [(Coord, Char)]
   , numbers :: [(Set Coord, Int)]
   }
readInput :: String -> Input
readInput ls = Input{..}
    where
    symbols = [ ((x,y), c)
              | (y, l) <- zip [0..] $ filter (not.null) $ lines ls
              , (x, c) <- zip [0..] l
              , c /= '.'
              , not $ isDigit c
              ]
    numbers = concat 
              [ readLine y $ zip [0..] l
              | (y, l) <- zip [0..] $ filter (not.null) $ lines ls
              ]

readLine :: Int -> [(Int, Char)] -> [(Set Coord, Int)]
readLine y [] = []
readLine y ((_,c):xs) | not (isDigit c) = readLine y xs
readLine y cs = 
    let (n, cs') = span (isDigit . snd) cs
        n' = read $ map snd n
        xs = map fst n
        s = Set.fromList $ map (,y) xs
     in (s, n') : readLine y cs'
    