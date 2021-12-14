-- See https://hackmd.io/@rkennedy/r11t5dI5Y

{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language TransformListComp #-}
module Main where

import Data.Foldable
import GHC.Exts (the, groupWith)

import Data.Matrix as M

__MOD = 100000009 -- 1000000 -> 28682688

main :: IO ()
main = interact $ show . flip mod __MOD . f' inputx . read
-- main = do
--     print $ part1 input `mod` 100000009
--     print $ part2 input `mod` 100000009

part1, part2 :: [Int] -> Integer
part1 c = f' c 80
part2 c = f' c 256

f c n = sum $ map snd $ iterate step (map (,1) c) !! n

step :: [(Int,Integer)] -> [(Int,Integer)]
step i = 
    [ (the p, sum l)
    | (p',l) <- i
    , p <- if p' == 0 then [6,8] else [p'-1]
    , then group by p using groupWith
    ]

input, inputx :: [Int]
input = [4,1,1,4,1,2,1,4,1,3,4,4,1,5,5,1,3,1,1,1,4,4,3,1,5,3,1,2,5,1,1,5,1,1,4,1,1,1,1,2,1,5,3,4,4,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,5,1,1,1,4,1,2,3,5,1,2,2,4,1,4,4,4,1,2,5,1,2,1,1,1,1,1,1,4,1,1,4,3,4,2,1,3,1,1,1,3,5,5,4,3,4,1,5,1,1,1,2,2,1,3,1,2,4,1,1,3,3,1,3,3,1,1,3,1,5,1,1,3,1,1,1,5,4,1,1,1,1,4,1,1,3,5,4,3,1,1,5,4,1,1,2,5,4,2,1,4,1,1,1,1,3,1,1,1,1,4,1,1,1,1,2,4,1,1,1,1,3,1,1,5,1,1,1,1,1,1,4,2,1,3,1,1,1,2,4,2,3,1,4,1,2,1,4,2,1,4,4,1,5,1,1,4,4,1,2,2,1,1,1,1,1,1,1,1,1,1,1,4,5,4,1,3,1,3,1,1,1,5,3,5,5,2,2,1,4,1,4,2,1,4,1,2,1,1,2,1,1,5,4,2,1,1,1,2,4,1,1,1,1,2,1,1,5,1,1,2,2,5,1,1,1,1,1,2,4,2,3,1,2,1,5,4,5,1,4]

inputx = [3,4,3,1,2]

f' :: [Int] -> Int -> Integer
f' c n = sum $ (adjacency `matExp` n) * toVec c

matExp :: Matrix Integer -> Int -> Matrix Integer
matExp m n = foldl1 (*) $ map snd $ filter fst $ zip (toDoubling n) $ iterate go m
    where
    go x = x*x
    toDoubling 0 = []
    toDoubling n = odd n : toDoubling (n `div` 2)

toVec :: [Int] -> Matrix Integer
toVec v = M.fromList 9 1 [fromIntegral $ length $ filter (==i) v | i <- [0..8] ]

adjacency :: Matrix Integer
adjacency = M.fromList 9 9
  [ f i j
  | i <- [0..8]
  , j <- [0..8]
  ]
  where
  f 6 0 = 1
  f 8 0 = 1
  f i j = if i == j-1 then 1 else 0
