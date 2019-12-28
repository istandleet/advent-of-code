{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.List
import Data.Array.Unboxed

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "19.txt" :: IO Program
    print $ p1 dat -- 231
    print $ p2 dat -- 9210745
    
p1 :: Program -> Int
p1 = length . filter id . elems . getBoard

p2 :: Program -> Int
p2 dat = 
    let initialSquare = (4,3) -- from investigation
        top = topLine (initComputer dat) initialSquare
        bot = botLine (initComputer dat) initialSquare
        (topix,botix) = findSquare 100 top bot
     in fst botix * 10000 + snd topix

getBoard :: Program -> Board
getBoard = investigateArea ((0,0),(49,49)) . initComputer
    
investigateArea :: (Coord,Coord) -> Computer -> Board
investigateArea bnds computer = array bnds $ map go (range bnds)
    where go ix = (ix,investigateSquare computer ix)
    
investigateSquare :: Computer -> Coord -> Bool
investigateSquare comp ix = ((==1) . head) $ evalState (interactSt [fst ix,snd ix]) comp
    
type Coord = (Int,Int)
type Board = UArray Coord Bool

topLine :: Computer -> Coord -> [Coord]
topLine comp = fix $ \rec ix -> ix :
    let nextsquares = iterate moveD (moveR ix)
     in rec $ head $ filter (investigateSquare comp) nextsquares
     
botLine :: Computer -> Coord -> [Coord]
botLine comp = fix $ \rec ix -> ix :
    let nextsquares = iterate moveR (moveD ix)
     in rec $ head $ filter (investigateSquare comp) nextsquares

findSquare :: Int -> [Coord] -> [Coord] -> (Coord,Coord)
findSquare w = go
    where
    go ((tx,ty):tops) ((bx,by):bots)
        | by < ty+pred w = go ((tx,ty):tops) bots
        | tx < bx+pred w = go tops ((bx,by):bots)
        | otherwise = ((tx,ty),(bx,by))
     
-- * Utils
drawBoard :: Board  -> String
drawBoard m = init $ unlines 
    [   [ if m !(x,y) then '#' else '.'
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds m
    
drawLines :: [Coord] -> [Coord] -> String
drawLines top bot = init $ unlines 
    [   [ if any (\(x',y') -> x' == x && y' <= y) top
          && any (\(x',y') -> x' <= x && y' == y) bot
            then '#' 
            else '.'
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    (x0,y0) = head top
    y1 = snd $ last bot
    x1 = fst $ last top
    
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)
moveR,moveD :: Coord -> Coord
moveR = _1 %~ succ
moveD = _2 %~ succ
    
-- * Testing
p1Main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "19.txt" :: IO Program
    let invs = getBoard dat
    putStrLn $ drawBoard invs
    print $ length . filter id . elems $ invs -- print $ p1 dat -- 231

p2Main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "19.txt" :: IO Program
    let initialSquare = (4,3) -- from investigation
        top = topLine (initComputer dat) initialSquare
        bot = botLine (initComputer dat) initialSquare
    putStrLn $ drawLines (take 50 top) (take 50 bot)
    print $ findSquare 2 (take 20 top) (take 20 bot)
    print $ p2 dat