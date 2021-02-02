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
    s <- readFile "12.txt"
    let input = map readLine $ filter (not.null) $ lines s
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Int
part1 = manhattanDist . _pos . foldl step initboat
    where initboat = Boat (Position 0 0) East

data Card = North | South | East | West
   deriving (Ord, Show, Eq, Bounded,Enum)
turnLeft = \case
    North -> West
    South -> East
    East  -> North
    West  -> South

data Action = N | S | E | W | L | R | F
   deriving (Ord, Show, Eq, Bounded,Enum, Read)

type Line = (Action, Int)

data Boat = Boat
   { _pos :: !Position
   , _dir :: !Card
   } deriving (Eq, Show)

step :: Boat -> Line -> Boat
step boat (a,n) = case a of 
    N -> boat & pos %~ ns %~ (+n)
    S -> boat & pos %~ ns %~ (subtract n)
    E -> boat & pos %~ ew %~ (+n)
    W -> boat & pos %~ ew %~ (subtract n)
    L -> case (n `div` 90) `mod` 4 of
        0 -> boat
        1 -> boat & dir %~ turnLeft
        2 -> boat & dir %~ turnLeft.turnLeft
        3 -> boat & dir %~ turnLeft.turnLeft.turnLeft
    R -> step boat (L,negate n)
    F -> step boat $ (,n) $ case _dir boat of 
        North -> N
        South -> S
        East  -> E
        West  -> W

data Position = Position
   { _ns :: !Int
   , _ew :: !Int
   } deriving (Eq, Show)
manhattanDist pos = abs(_ns pos) + abs(_ew pos)

type BoatAndWaypoint = (Position, Position) -- (boat, waypoint)

part2 :: [Line] -> Int
part2 = manhattanDist . fst . foldl step' initst
    where
    initst = (Position 0 0, Position 1 10)

step' :: BoatAndWaypoint -> Line -> BoatAndWaypoint
step' (boat, waypoint) (a,n) = case a of 
    N -> (boat, waypoint & ns %~ (+n))
    S -> (boat, waypoint & ns %~ subtract n)
    E -> (boat, waypoint & ew %~ (+n))
    W -> (boat, waypoint & ew %~ subtract n)
    L -> (boat,) $ case (n `div` 90) `mod` 4 of
        0 -> waypoint
        1 -> waypoint & rotateWaypointL
        2 -> waypoint & rotateWaypointL.rotateWaypointL
        3 -> waypoint & rotateWaypointL.rotateWaypointL.rotateWaypointL
    R -> step' (boat, waypoint) (L,negate n)
    F -> (,waypoint) $
            let east  = (*n) $ _ew waypoint
                north = (*n) $ _ns waypoint
             in boat & ns %~ (+north) & ew %~ (+east)
    where
    rotateWaypointL (Position ns ew) = Position ew (negate ns)

readLine :: String -> Line
readLine (a:n) = (read [a], read n)

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Boat 
-- makeLenses ''Position

dir :: Lens' Boat Card
dir f_amTx (Boat x1_amTy x2_amTz)
    = (fmap (\ y1_amTA -> (Boat x1_amTy) y1_amTA)) (f_amTx x2_amTz)
{-# INLINE dir #-}
pos :: Lens' Boat Position
pos f_amTB (Boat x1_amTC x2_amTD)
    = (fmap (\ y1_amTE -> (Boat y1_amTE) x2_amTD)) (f_amTB x1_amTC)
{-# INLINE pos #-}
ew :: Lens' Position Int
ew f_amUn (Position x1_amUo x2_amUp)
    = (fmap (\ y1_amUq -> (Position x1_amUo) y1_amUq)) (f_amUn x2_amUp)
{-# INLINE ew #-}
ns :: Lens' Position Int
ns f_amUr (Position x1_amUs x2_amUt)
    = (fmap (\ y1_amUu -> (Position y1_amUu) x2_amUt)) (f_amUr x1_amUs)
{-# INLINE ns #-}