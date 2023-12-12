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
    input <- readInput <$> readFile "10.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 (s, pipes) = furthest (Map.fromList pipes) s
part2 (s, pipes) = contained' $ enlargePipemap (Map.fromList pipes) $ connected (Map.fromList pipes) s

furthest :: Map Coord Pipe -> Coord -> Int
furthest m c = go 0 mempty $ Set.singleton c
    where
    go !n !seen !frontier
        | Set.null frontier = pred n
        | otherwise = 
            let seen' = seen <> frontier
                frontier' = Set.fromList 
                    [ c'
                    | c <- Set.toList frontier
                    , let p = m Map.! c
                          (a, b) = connections p c
                    , c' <- [a, b]
                    ]
             in go (n+1) seen' (frontier' Set.\\ seen')

contained :: Set Coord -> Int
contained wall = sum $ map (length . Set.filter possible) 
                $ filter (all $ inRange ((x0,y0),(x1,y1))) 
                $ glomp 
                $ Set.fromList
                    [ (x,y) 
                    | x <- [x0-1..x1+1]
                    , y <- [y0-1..y1+1]
                    , (x,y) `Set.notMember` wall
                    ]
    where
    (x0, x1) = (\s -> (minimum s, maximum s)) $ map fst $ Set.toList wall
    (y0, y1) = (\s -> (minimum s, maximum s)) $ map snd $ Set.toList wall
    possible (x,y) = even x && even y && (x,y) `Set.notMember` wall

glomp :: Set Coord -> [Set Coord] -- glomp together all the connected components
glomp s = maybe [] (uncurry $ go mempty . Set.singleton) $ Set.minView s
    where
    go acc this remaining =
        let new = Set.intersection remaining $ foldMap neighbors this Set.\\ acc
         in if not (null new)
                then go (acc <> this <> new) new (remaining Set.\\ new)
                else acc : glomp remaining

connected :: Map Coord Pipe -> Coord -> Set Coord
connected m c = go mempty $ Set.singleton c
    where
    go !seen !frontier
        | Set.null frontier = seen
        | otherwise = 
            let seen' = seen <> frontier
                frontier' = Set.fromList 
                    [ c'
                    | c <- Set.toList frontier
                    , let p = m Map.! c
                          (a, b) = connections p c
                    , c' <- [a, b]
                    ]
             in go seen' (frontier' Set.\\ seen')

doubleCoords :: Coord -> Coord
doubleCoords (x,y) = (x*2, y*2)

enlargePipemap :: Map Coord Pipe -> Set Coord -> Set Coord
-- we double all the coordinates and fill in with the pipe connectors
enlargePipemap m = foldMap go
    where
    go c = let c' = doubleCoords c
               p = m Map.! c
               (a, b) = connections p c'
            in Set.fromList [c', a, b]


type Coord = (Int, Int)
type Input = (Coord, [(Coord, Pipe)])
readInput :: String -> Input
readInput s = (start, pipes)
    where
    pipes = 
        [ ((x, y), p)
        | (y, l) <- zip [0..] $ lines s
        , (x, c) <- zip [0..] l
        , p <- maybeToList $ readPipe c
        ]
    start = head
        [ (x, y)
        | (y, l) <- zip [0..] $ lines s
        , (x, c) <- zip [0..] l
        , c == 'S'
        ]

data Pipe
   = Vert
   | Horiz
   | L
   | J
   | F 
   | Seven
   deriving (Show, Eq, Ord, Enum, Bounded)

connections :: Pipe -> Coord -> (Coord, Coord)
connections p (x,y) = case p of
    Vert -> ((x,y-1), (x,y+1))
    Horiz -> ((x-1,y), (x+1,y))
    L -> ((x,y-1), (x+1,y))
    J -> ((x,y-1), (x-1,y))
    F -> ((x+1,y), (x,y+1))
    Seven -> ((x-1,y), (x,y+1))

readPipe :: Char -> Maybe Pipe
readPipe 'S' = Just J -- visual analysis
readPipe c = lookup c $ zip "|-LJF7" [Vert ..]

-- * Utils
neighbors :: Coord -> Set Coord
neighbors (x,y) = Set.fromDistinctAscList
    [ (x-1,y)
    , (x,y-1)
    , (x,y+1)
    , (x+1,y)
    ]
