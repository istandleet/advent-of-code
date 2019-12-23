{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import GHC.Exts (groupWith,the)

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "6"
    s <- lines <$> readFile "6.txt"
    putStrLn $ part1 s
    putStrLn $ part2 s

part1 :: [String] -> String
part1 ss =
    [ mostCommon [s !! i | s <- ss]
    | i <- [0..length (head ss)-1]
    ]
part2 :: [String] -> String
part2 ss =
    [ leastCommon [s !! i | s <- ss]
    | i <- [0..length (head ss)-1]
    ]
    
mostCommon :: Ord a => [a] -> a
mostCommon xs = fst $ head $ Data.List.sortOn (Down . snd) $ count xs
leastCommon :: Ord a => [a] -> a
leastCommon xs = fst $ head $ Data.List.sortOn snd $ count xs
    
    
count xs = [(the x, length x) | x <- xs, then group by x using groupWith]