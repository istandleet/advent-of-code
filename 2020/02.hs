{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language RecordWildCards #-}
module Main where

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    s <- readFile "02.txt"
    let input = map readLine $ filter (not . null) $ lines s
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Int
part1 = length . filter valid1

valid1 :: Line -> Bool
valid1 Line{..} = 
    let occurences = length $ filter (==letter) password
     in maxNum >= occurences && minNum <= occurences

part2 :: [Line] -> Int
part2 = length . filter valid2

valid2 :: Line -> Bool
valid2 Line{..} = 
    let a = (password !! (minNum-1)) == letter
        b = (password !! (maxNum-1)) == letter
     in a `xor` b

data Line = Line 
   { maxNum :: !Int
   , minNum :: !Int
   , letter :: !Char
   , password :: !String
   }

-- Parsing
readLine :: String -> Line
readLine s =
    let [a,b,password] = words s
        letter = head b
        (c,d) = break (=='-') a
        minNum = read c
        maxNum = read $ tail d
     in Line{..}
