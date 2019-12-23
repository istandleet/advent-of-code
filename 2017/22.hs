{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "22"
    b <- readInput <$> readFile "22.txt"
    print $ part1 b
    
    
s = "..#\n#..\n..."

part1 :: Board -> Int
part1 b = let (d,c,b',n') = part1'' 10000 b in n'

part1'' :: Int -> Board -> (Dir,Coord,Board,Int)
part1'' n b = execState (replicateM n $ modify' step) (U,(0,0),b,0)

part1' :: Int -> Board -> IO ()
part1' n b = let (d,c,b',n') = part1'' n b in putStrLn (showBoard b') >> print n'

type Coord = (Int,Int)
type Board = Set Coord
data Dir = U | L | R | D deriving (Eq, Show, Ord)
turn :: Bool -> Dir -> Dir
turn isright d = case d of
    U -> if isright then R else L
    L -> if isright then U else D
    R -> if isright then D else U
    D -> if isright then L else R
move :: Dir -> Coord -> Coord
move dir = case dir of 
    U -> _2 %~ succ
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ pred
    
step :: (Dir,Coord,Board,Int) -> (Dir,Coord,Board,Int)
step (d,c,b,n) = (d',move d' c,b',n')
    where
    n' = if isinfected then n else succ n
    b' = (if isinfected then Set.delete else Set.insert) c b
    d' = turn isinfected d
    isinfected = c `Set.member` b
    
readInput :: String -> Board
readInput = Set.fromList . go . lines 
    where
    go ls = let ysize = length ls
                xsize = length (head ls)
                ymax = ysize `div` 2
                xmax = xsize `div` 2
             in [ (x,-y)
                | (y,l) <- zip [-ymax..ymax] ls
                , (x,c) <- zip [-xmax..xmax] l
                , c == '#'
                ]
                
showBoard :: Board -> String
showBoard b = 
    let ls = Set.elems b 
        xmin = minimum $ map fst ls
        xmax = maximum $ map fst ls
        ymin = minimum $ map snd ls
        ymax = maximum $ map snd ls
     in unlines [[if Set.member (x,y) b then '#' else '.'| x <- [xmin..xmax]] | y <- [ymin..ymax]]