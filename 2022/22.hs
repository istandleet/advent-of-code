{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
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

main :: IO ()
main = do
    input <- readInput <$> readFile "22.txt"
    print $ part1 input

part1 :: Input -> Int
part1 Input{..} = finalize $ foldl' (move floor' walls) ((x0,1), R) dirs
    where
    x0 = minimum $ Set.map fst $ Set.filter ((==1).snd) floor'

finalize :: Pos -> Int
finalize ((x,y), d) = 1000 * y + 4 * x + case d of
    U -> 3
    D -> 1
    L -> 2
    R -> 0

data Input = Input
    { walls :: Walls
    , floor' :: Floor
    , dirs :: [Move]
    } deriving (Show)

type Coord = (Int, Int)
type Walls = Set Coord
type Floor = Set Coord
type Move = Either Int Dir 
data Dir = U | D | L | R deriving (Show)

type Pos = (Coord, Dir)

move :: Floor -> Walls -> Pos -> Move -> Pos
move floor walls = go
    where
    go (c, d) (Right d') = (c, turn d' d)
    go (c, d) (Left 0) = (c, d)
    go ((x,y), d) (Left n) = 
        if newc `Set.member` walls
            then ((x,y),d)
            else go (newc, d) (Left (n-1))
        where
        c' = case d of
            U -> (x, y-1)
            D -> (x, y+1)
            L -> (x-1, y)
            R -> (x+1, y)
        newc = if c' `Set.member` floor 
                then c' 
                else case d of
                    U -> (x, maximum $ Set.map snd $ Set.filter ((==x).fst) floor)
                    D -> (x, minimum $ Set.map snd $ Set.filter ((==x).fst) floor)
                    L -> (maximum $ Set.map fst $ Set.filter ((==y).snd) floor, y)
                    R -> (minimum $ Set.map fst $ Set.filter ((==y).snd) floor, y)

turn :: Dir -> Dir -> Dir
turn L = \case
    U -> L
    D -> R
    L -> D
    R -> U
turn R = \case
    U -> R
    D -> L
    L -> U
    R -> D
turn _ = error "turn"

-- * Parsing
readInput :: String -> Input
readInput s = Input{..}
    where
    dirs = parseDirs $ last $ filter (not.null) $ lines s
    area = 
        [ ((x, y), c == '#')
        | (y, l) <- zip [1..] $ takeWhile (not.null) $ lines s
        , (x, c) <- zip [1..] l
        , c `elem` ['#','.']
        ]
    floor' = Set.fromList $ map fst area
    walls = Set.fromList $ map fst $ filter snd area

parseDirs :: String -> [Move]
parseDirs [] = []
parseDirs ('L':xs) = Right L : parseDirs xs
parseDirs ('R':xs) = Right R : parseDirs xs
parseDirs xs = Left (read n) : parseDirs xs'
  where (n, xs') = span isDigit xs
