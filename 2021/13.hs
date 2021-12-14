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
    cs <- getC <$> readFile "13p.txt"
    es <- getE <$> readFile "13e.txt"
    -- mapM_ (putStrLn . drawBoard) $ scanl step (Set.fromList cs) es
    print $ part1 es cs
    putStrLn $ drawBoard $ foldl step (Set.fromList cs) es

part1 :: [E] -> [Coord] -> Int
part1 es cs = length $ step (Set.fromList cs) $ head es
part2 es cs = length $ foldl step (Set.fromList cs) es

step :: Set Coord -> E -> Set Coord
step s (True , d) = Set.map (\(x,y) -> (if x > d then d - (x-d) else x, y)) s
step s (False, d) = Set.map (\(x,y) -> (x,if y > d then d - (y-d) else y)) s

type Coord = (Int, Int)
getC :: String -> [Coord]
getC = map (\(a,b) -> (read a, read b)) . map go . filter (not . null) . lines 
    where go = fmap tail . break (==',')

type E = (Bool, Int)
getE :: String -> [E]
getE = map (\(a,b) -> (r $ last a, read b)) . map go . filter (not . null) . lines 
    where 
    go = fmap tail . break (=='=')
    r c = c == 'x'

drawBoard :: Set Coord -> String
drawBoard board = unlines
    [[tc $ (x,y) `Set.member` board | x <- [xmin..xmax]]
    | y <- [ymin..ymax]
    ]
    where
    xmin = minimum $ Set.map fst board
    xmax = maximum $ Set.map fst board
    ymin = minimum $ Set.map snd board
    ymax = maximum $ Set.map snd board
    tc b = if b then '#' else '.'
