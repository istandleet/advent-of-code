{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Array.Unboxed
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "16"
    putStrLn $ sshow $ part1 272 s
    putStrLn $ sshow $ part1 35651584 s
    
s = unshow "11011110011011101"
    
part1 :: Int -> [Bool] -> [Bool]
part1 n = checksum . dragonTil n
    
dragonTil :: Int -> [Bool] -> [Bool]
dragonTil n bs = let bs' = dragon bs in if length bs' >= n then take n bs' else dragonTil n bs'
dragon :: [Bool] -> [Bool]
dragon a = 
    a ++ [False] ++ reverse (map not a)
    
checksum :: [Bool] -> [Bool]
checksum bs = let bs' = checksum' bs in if odd $ length bs' then bs' else checksum bs'

checksum' :: [Bool] -> [Bool]
checksum' a =
    [ x == y
    | (i,(x,y)) <- zip [0..] $ zip a (tail a)
    , even i
    ]
    
sshow :: [Bool] -> String
sshow = map $ \b -> if b then '1' else '0'

unshow :: String -> [Bool] 
unshow = map (=='1')