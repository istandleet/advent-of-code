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
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as UV
import qualified Data.List
import Data.Array
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    s <- readFile "11.txt"
    let input = readBoard s
    print $ part1 input
    print $ part2 $ readBoard' s

part1 :: Board -> Int
part1 = countTrue . occupied . fst . head
      . dropWhile (uncurry (/=)) 
      . (\bs -> zip bs (tail bs)) 
      . iterate step

step :: Board -> Board
step Board{..} = Board seats occupied'
    where
    bnds = bounds seats
    occupied' = array bnds
        [ (ix, (seats!ix) && (if occupied!ix then nocc<4 else nocc==0))
        | ix <- range bnds
        , let nocc = length $ filter (occupied!) $ filter (inRange bnds) $ neighbors ix
        ]

data Board = Board
   { seats :: Array Coord Bool
   , occupied :: Array Coord Bool
   } deriving (Eq, Show)

readBoard :: String -> Board
readBoard s = Board{..}
    where
    ymax = pred $ length $ filter (not.null) $ lines s
    xmax = pred $ length $ head $ lines s
    bnds = ((0,0),(xmax,ymax))
    occupied = listArray bnds $ replicate (succ xmax*succ ymax) False
    seats = array bnds
        [ ((x,y), c == 'L')
        | (y,l) <- zip [0..] $ filter (not.null) $ lines s
        , (x,c) <- zip [0..] l
        ]

drawBoard :: Board -> String
drawBoard Board{..} = unlines
    [ [if not (seats!ix) then '.' else if occupied ! ix then '#' else 'L' | x <- xs, let ix = (x,the y)]
    | (xs,y) <- range $ bounds seats
    , then group by y using groupWith
    ]


part2 :: Board' -> Int
part2 = countTrue . occupied' . fst . head
      . dropWhile (uncurry ((/=) `on` occupied')) 
      . (\bs -> zip bs (tail bs)) 
      . iterate step'

step' :: Board' -> Board'
step' Board'{..} = Board' seats' occupied''
    where
    bnds = bounds seats'
    occupied'' = array bnds
        [ (ix, nowocc)
        | ix <- range bnds
        , let nowocc = case seats' ! ix of
                Nothing -> False
                Just seats -> 
                    let nocc = UV.length $ UV.filter (occupied'!) seats
                     in if occupied'!ix then nocc<5 else nocc==0
        ]

data Board' = Board'
   { seats' :: Array Coord (Maybe (UV.Vector Coord))
   , occupied' :: Array Coord Bool
   } deriving (Eq, Show)

readBoard' :: String -> Board'
readBoard' s = Board'{..}
    where
    ymax = pred $ length $ filter (not.null) $ lines s
    xmax = pred $ length $ head $ lines s
    bnds = ((0,0),(xmax,ymax))
    occupied' = listArray bnds $ replicate (succ xmax*succ ymax) False
    seats = array bnds
        [ ((x,y), c == 'L')
        | (y,l) <- zip [0..] $ filter (not.null) $ lines s
        , (x,c) <- zip [0..] l
        ]
    seats' = array bnds
        [ ((x,y), goseats (x,y))
        | (y,l) <- zip [0..] $ filter (not.null) $ lines s
        , (x,c) <- zip [0..] l
        ]
    goseats ix = if not (seats!ix) then Nothing else Just $ UV.fromList
        [ head seatexists
        | c <- cardinalities
        , let ixs = takeWhile (inRange bnds) $ tail $ iterate (adjust c) ix
        , let seatexists = filter (seats!) ixs
        , not $ null seatexists
        ]

-- * Utils
countTrue :: Ix ix => Array ix Bool -> Int
countTrue = length . filter id . elems

type Coord = (Int,Int)

neighbors :: Coord -> [Coord]
neighbors ix = map (`adjust` ix) cardinalities

adjust :: (Int,Int) -> Coord -> Coord
adjust (dx,dy) (x,y) = (x+dx, y+dy)
cardinalities :: [(Int,Int)]
cardinalities = 
    [ (x',y')
    | x' <- [-1,0,1]
    , y' <- [-1,0,1]
    , (0,0) /= (x',y')
    ]
