{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Word
import qualified Data.List
import qualified Data.Vector as Vector
import qualified Data.Set as Set

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "13"
    is <- getInput
    print $ part1 $ mkScanners is
    print $ part2 is

getInput :: IO [(Int,Int)]
getInput = map readInput . lines <$> readFile "13.txt"
readInput = bimap read (read . tail) . break (==':')
    
s :: [(Int,Int)]
s = [(0,3),(1,2),(4,4),(6,4)]

part1 :: Board -> Int
part1 b = sum $ evalState (mapM go [0..length b - 1]) b
    where
    go i = do
        sc <- (Vector.! i) <$> get
        let r = case sc of
                    Nothing -> 0
                    Just s -> if pos s == 0 then range s * i else 0
        modify' move
        return r

solves :: [(Int,Int)] -> Int -> Bool
solves is n = all go is
    where
    go (i,m) = ((n + i) `mod` (2*(m-1))) /= 0
        
part2 :: [(Int,Int)] -> Int
part2 is = head $ filter (solves is) [0..]

type Board = Vector.Vector (Maybe Scanner)

move :: Board -> Board
move = fmap (fmap go)
    where
    go (Scanner p r m)
        | m && p == r-1 = Scanner (p-1) r False
        | m = Scanner (p+1) r m
        | not m && p == 0 = Scanner 1 r True
        | not m = Scanner (p-1) r m

mkScanners :: [(Int,Int)] -> Board
mkScanners is = Vector.generate (succ $ maximum $ map fst is) go
    where
    go i = (\range -> Scanner 0 range True) <$> lookup i is

data Scanner = Scanner {
     pos        :: {-# unpack #-} !Int, 
     range      :: {-# unpack #-} !Int, 
     movingDown :: !Bool
     } deriving (Eq, Show)