{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text hiding (D)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "2"
    input <- getInput
    putStrLn $ part1 input
    putStrLn $ part2 input

data Dir = U | L | R | D deriving (Eq,Ord,Show,Read)
type Line = [Dir]
    
part1 :: [Line] -> String
part1 ls = evalState (mapM go ls) '5'
    where
    go ds = mapM_ (modify . move) ds >> get
part2 :: [Line] -> String
part2 ls = evalState (mapM go ls) '5'
    where
    go ds = mapM_ (modify . move') ds >> get
    
move :: Dir -> Char -> Char
move U c = case c of 
    '1' -> '1'
    '2' -> '2'
    '3' -> '3'
    '4' -> '1'
    '5' -> '2'
    '6' -> '3'
    '7' -> '4'
    '8' -> '5'
    '9' -> '6'
move D c = case c of 
    '1' -> '4'
    '2' -> '5'
    '3' -> '6'
    '4' -> '7'
    '5' -> '8'
    '6' -> '9'
    '7' -> '7'
    '8' -> '8'
    '9' -> '9'
move R c = case c of 
    '1' -> '2'
    '2' -> '3'
    '3' -> '3'
    '4' -> '5'
    '5' -> '6'
    '6' -> '6'
    '7' -> '8'
    '8' -> '9'
    '9' -> '9'
move L c = case c of 
    '1' -> '1'
    '2' -> '1'
    '3' -> '2'
    '4' -> '4'
    '5' -> '4'
    '6' -> '5'
    '7' -> '7'
    '8' -> '7'
    '9' -> '8'
    
move' :: Dir -> Char -> Char
move' U c = case c of 
    '1' -> '1'
    '2' -> '2'
    '3' -> '1'
    '4' -> '4'
    '5' -> '5'
    '6' -> '2'
    '7' -> '3'
    '8' -> '4'
    '9' -> '9'
    'A' -> '6'
    'B' -> '7'
    'C' -> '8'
    'D' -> 'B'
move' D c = case c of 
    '1' -> '3'
    '2' -> '6'
    '3' -> '7'
    '4' -> '8'
    '5' -> '5'
    '6' -> 'A'
    '7' -> 'B'
    '8' -> 'C'
    '9' -> '9'
    'A' -> 'A'
    'B' -> 'D'
    'C' -> 'C'
    'D' -> 'D'
move' R c = case c of 
    '1' -> '1'
    '2' -> '3'
    '3' -> '4'
    '4' -> '4'
    '5' -> '6'
    '6' -> '7'
    '7' -> '8'
    '8' -> '9'
    '9' -> '9'
    'A' -> 'B'
    'B' -> 'C'
    'C' -> 'C'
    'D' -> 'D'
move' L c = case c of 
    '1' -> '1'
    '2' -> '2'
    '3' -> '2'
    '4' -> '3'
    '5' -> '5'
    '6' -> '5'
    '7' -> '6'
    '8' -> '7'
    '9' -> '8'
    'A' -> 'A'
    'B' -> 'A'
    'C' -> 'B'
    'D' -> 'D'
    
    
getInput :: IO [Line]
getInput = map (map $ read . pure) . lines <$> readFile "2.txt"

s = "ULL\nRRDDD\nLURDL\nUUUUD"