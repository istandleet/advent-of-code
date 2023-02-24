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

main :: IO ()
main = do
    input <- readInput <$> readFile "24.txt"
    print $ part1 input
    print $ part2 input

part1 :: Board -> Int
part1 = fromJust . Data.List.findIndex done . iterate move
    where done b = (width b - 1, height b) `Set.member` pos b
part2 :: Board -> Int
part2 b =
    let (a, b1:_) = break done $ iterate move b
        b2 = b1{pos = Set.singleton (width b1 - 1, height b1)}
        (a', b3:_) = break start $ iterate move b2
        b4 = b3{pos = Set.singleton (0, -1)}
        (a'', _) = break done $ iterate move b4
     in length a + length a' + length a''
    where 
    done b = (width b - 1, height b) `Set.member` pos b
    start b = (0,-1) `Set.member` pos b
    
    


data Board = Board
   { pos    :: !(Set Coord)
   , left   :: !(Set Coord)
   , right  :: !(Set Coord)
   , up     :: !(Set Coord)
   , down   :: !(Set Coord)
   , height :: {-# unpack #-} !Int
   , width  :: {-# unpack #-} !Int
   } deriving (Show)
type Coord = (Int, Int)


legal :: Board -> Coord -> Bool
legal Board{..} = \(x,y) -> 
       (x >= 0 && x < width && y >= 0 && y < height)
    || (x == 0 && y == -1)
    || (x == width-1 && y == height)

move :: Board -> Board 
move b@Board{..} = b 
    { pos = pos'
    , left = left'
    , right = right'
    , up = up'
    , down = down'
    }
    where
    left'  = Set.map (first  $ \x -> (x-1) `mod` width ) left
    right' = Set.map (first  $ \x -> (x+1) `mod` width ) right
    up'    = Set.map (second $ \y -> (y-1) `mod` height) up
    down'  = Set.map (second $ \y -> (y+1) `mod` height) down
    posspos = Set.filter (legal b) $ foldMap neighbors pos
    pos' = posspos `Set.difference` (left' <> right' <> up' <> down')

neighbors :: Coord -> Set Coord
neighbors (x,y) = Set.fromDistinctAscList
    [ (x-1,y)
    , (x,y-1)
    , (x,y)
    , (x,y+1)
    , (x+1,y)
    ]

rotate [] = []
rotate (x:xs) = xs ++ [x]

-- * Parsing
readInput :: String -> Board
readInput s = Board{..}
    where
    pos = Set.singleton (0,-1)
    ls = map (init . tail) $ init . tail $ filter (not.null) $ lines s
    height = length ls
    width = length $ head ls
    left = Set.fromList 
        [ (x,y) 
        | (y, l) <- zip [0..] ls
        , (x, c) <- zip [0..] l
        , c == '<'
        ]
    right = Set.fromList 
        [ (x,y)
        | (y, l) <- zip [0..] ls
        , (x, c) <- zip [0..] l
        , c == '>'
        ]
    up = Set.fromList 
        [ (x,y)
        | (y, l) <- zip [0..] ls
        , (x, c) <- zip [0..] l
        , c == '^'
        ]
    down = Set.fromList 
        [ (x,y)
        | (y, l) <- zip [0..] ls
        , (x, c) <- zip [0..] l
        , c == 'v'
        ]
