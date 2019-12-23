{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-}
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "22b"
    print $ part2 $ readInput s
    b <- readInput <$> readFile "22.txt"
    print $ part2 b
    
    
s = "..#\n#..\n..."

part2 :: Board -> Int
part2 b = let (d,c,b',n') = part2'' 10000000 b in n'

part2'' :: Int -> Board -> (Dir,Coord,Board,Int)
part2'' n b = iterate step (U,(0,0),b,0) !! n

part2' :: Int -> Board -> IO ()
part2' n b = let (d,c,b',n') = part2'' n b in putStrLn (showBoard b') >> print n'

type Coord = (Int,Int)
data InfState = C | W | I | F deriving (Eq, Show, Ord)
nextis :: InfState -> InfState
nextis = \case
    C -> W
    W -> I
    I -> F
    F -> C

type Board = Map Coord InfState
data Dir = U | L | R | D deriving (Eq, Show, Ord)
oppo :: Dir -> Dir
oppo = \case
    U -> D
    L -> R
    R -> L
    D -> U
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
step (!d,!c,!b,!n) = (d',move d' c,b',n')
    where
    n' = if infstate' == I then succ n else n
    b' = case infstate' of
        C -> Map.delete c b
        _ -> Map.insert c infstate' b
    d' = dirf infstate d
    infstate = fromMaybe C $ Map.lookup c b
    infstate' = nextis infstate
    
dirf :: InfState -> Dir -> Dir
dirf = \case
    C -> turn False
    W -> id
    I -> turn True
    F -> oppo
    
readInput :: String -> Board
readInput = Map.fromList . go . lines 
    where
    go ls = let ysize = length ls
                xsize = length (head ls)
                ymax = ysize `div` 2
                xmax = xsize `div` 2
             in [ ((x,-y),I)
                | (y,l) <- zip [-ymax..ymax] ls
                , (x,c) <- zip [-xmax..xmax] l
                , c == '#'
                ]
                
showBoard :: Board -> String
showBoard b = 
    let ls = Map.keys b 
        xmin = minimum $ map fst ls
        xmax = maximum $ map fst ls
        ymin = minimum $ map snd ls
        ymax = maximum $ map snd ls
     in unlines [[if not (Map.member (x,y) b) then 'C' else head (show $ b Map.! (x,y))| x <- [xmin..xmax]] | y <- [ymin..ymax]]