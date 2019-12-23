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
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import Numeric (showHex)

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "10"
    print $  part1 255 s
    putStrLn $ part2 s'
    
s :: [Int]
s = map (read . T.unpack) $ T.split (==',') "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
s' :: String
s' = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"


part1 :: Int -> [Int] -> Int
part1 n ls = let (a,b,c) = part1' n ls in c Vector.! 0 * c Vector.! 1
part1' :: Int -> [Int] -> (Int,Int,Vector Int)
part1' n ls = execState (part1M ls) (0,0,Vector.fromList [0..n])
    
part1M :: [Int] -> State (Int,Int,Vector Int) ()
part1M = mapM_ (modify' . go)
    where
    go :: Int -> (Int,Int,Vector Int) -> (Int,Int,Vector Int)
    go l (p,s,v) = ((p + l + s) `mod` length v,s+1,move p l v)

part2 :: String -> String
part2 s = mconcat $ map showHex' $ 
    let (a,b,c) = execState (replicateM 64 $ part1M ls) (0,0,Vector.fromList [0..255]) in go c
    where
    ls = map ord s ++ [17, 31, 73, 47, 23]
    go v = foldl1 xor <$> splitEvery 16 v
    
    
splitEvery :: Int -> Vector a -> [Vector a]
splitEvery n v 
    | null v = [] 
    | otherwise = let (a,b) = Vector.splitAt n v in a : splitEvery n b

showHex' :: Int -> String
showHex' n 
    | n < 16 = '0' : showHex n ""
    | otherwise = showHex n ""
    
-- move 0 3 (Vector.fromList [0..4]) == Vector.fromList [2,1,0,3,4]
-- move 3 4 (Vector.fromList [2,1,0,3,4]) == Vector.fromList [4,3,0,1,2]
-- move 1 5 (Vector.fromList [4,3,0,1,2]) == Vector.fromList [3,4,2,1,0]
move :: Int -> Int -> Vector Int -> Vector Int 
move _ l v | l <= 1 = v
move p l v | p + l <= length v = Vector.force $ a <> Vector.reverse b <> c
    where
    (a,v') = Vector.splitAt p v
    (b,c) = Vector.splitAt l v'
move p l v = Vector.force $ a' <> b <> c'
    where
    (v',c) = Vector.splitAt p v
    (a,b) = Vector.splitAt (l - Vector.length c) v'
    v'' = Vector.reverse $ c <> a
    (c',a') = Vector.splitAt (length c) v''