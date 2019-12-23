{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Char
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Char
import Data.Attoparsec.Text hiding (D, count)
import Data.Text (Text)
import qualified Data.Text as T




main = do
    putStrLn "19"
    board <- readInput <$> readFile "19.txt"
    let (n,s,c,d) = part1 board 
    putStrLn $ reverse s
    print n

    
    
type St = (Int,String,Coord,Dir)

part1 :: Board -> St
part1 board = go (0,"",startingPosition $ snd board, U)
    where
    changer = change board
    go a = maybe a go $ changer a 

change :: Board -> St -> Maybe St
change (cs,a) (n,s,co,dir) = do
    (dir',co') <- findMove a dir co
    let s' = case Map.lookup co' cs of 
                Nothing -> s
                Just c  -> c:s
    pure (n+1,s',co',dir')
    
findMove :: Track -> Dir -> Coord -> Maybe (Dir,Coord)
findMove t dir co = listToMaybe $ mapMaybe go [dir, turn True dir, turn False dir]
    where
    bnds = bounds t
    go d = let m = move d co in (d,m) <$ guard (inRange bnds m && t!m)


    
type Track = UArray Coord Bool
type Board = (Map Coord Char, Track)

type Coord = (Int,Int)
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
    
readInput :: String -> Board
readInput s = (chars,arr)
    where
    bnds = ((0,0),(length $ head $ lines s,length $ lines s))
    arr = array bnds [ ((x,y),c/=' ')
          | (y,l) <- zip [0..]$ lines s
          , (x,c) <- zip [0..] l
          ]
    chars = Map.fromList [ ((x,y),c)
          | (y,l) <- zip [0..]$ lines s
          , (x,c) <- zip [0..] l
          , isAlpha c
          ]
          
ex :: String
ex ="     |          \n\
    \     |  +--+    \n\
    \     A  |  C    \n\
    \ F---|----E|--+ \n\
    \     |  |  |  D \n\
    \     +B-+  +--+ "

startingPosition :: Track -> Coord
startingPosition a = head
    [(x,0)| x <- [x1..x2],a!(x,0)]
    where
    ((x1,_),(x2,_)) = bounds a