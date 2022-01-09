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
    input <- getInput <$> readFile "20.txt" :: IO Input
    print $ part1 input -- 5819
    print $ part2 input

part1, part2 :: Input -> Int
part1 (iea, b) = length $ filter id $ elems $ snd $ iterate (step iea) (False, b) !! 2
part2 (iea, b) = length $ filter id $ elems $ snd $ iterate (step iea) (False, b) !! 50

step :: IEA -> (Bool, Board) -> (Bool, Board)
step iea (background, board) = (if UV.head iea then not background else background,) $ array newbounds
    [ (c, iea UV.! fromdigits (map f $ neighbors c) )
    | c <- range newbounds
    ]
    where 
    ((xmin,ymin),(xmax,ymax)) = bounds board
    newbounds = ((xmin-1,ymin-1),(xmax+1,ymax+1))
    f ix = if inRange (bounds board) ix then board ! ix else background

newValue :: Board -> Coord -> [Bool]
newValue board ix = map f $ neighbors ix
    where f ix = inRange (bounds board) ix && board ! ix

type Board' = Set Coord 
step' :: IEA -> Board' -> Board' 
step' iea board = Set.fromList 
    [ ix
    | x <- [minimum xs - 1..maximum xs + 1]
    , y <- [minimum ys - 1..maximum ys + 1]
    , let ix = (x,y)
          bs = map (`Set.member` board) $ neighbors ix
    , iea UV.! fromdigits bs
    ]
    where 
    xs = Set.map fst board
    ys = Set.map snd board

fromdigits :: [Bool] -> Int
fromdigits = foldl (\b a -> 2*b + if a then 1 else 0) 0

type Coord = (Int,Int)
type IEA = UV.Vector Bool
type Input = (IEA, Board)
type Board = UArray Coord Bool

neighbors :: Coord -> [Coord]
neighbors (x,y) =
    [ (x+dx,y+dy)
    | dy <- [-1..1] 
    , dx <- [-1..1]
    ]

getInput :: String -> Input
getInput s = let (a:_:as) = lines s in (UV.fromList $ map readChar a,readBoard as)

readChar c = c == '#'
readBoard :: [String] -> Board
readBoard s = array ((0,0),(xmax, ymax))
    [ ((x,y), readChar c)
    | (y,l) <- zip [0..] $ filter (not . null) s
    , (x,c) <- zip [0..] l
    ] 
    where
    ymax = pred $ length $ filter (not . null) s
    xmax = pred $ length $ head s

drawBoard :: Board -> String
drawBoard board = unlines
    [[tc $ board ! (x,y) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]
    ]
    where
    ((xmin,ymin),(xmax,ymax)) = bounds board
    tc b = if b then '#' else '.'