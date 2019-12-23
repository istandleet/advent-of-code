{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language TupleSections #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Parallel.Strategies
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.List
import Data.Tuple (swap)
import Data.Maybe
import System.Environment

main = do
    arg <- getArgs
    print arg
    let [n] = map read arg
    s <- filter isDigit <$> readFile "16.txt"
    let is = map digitToInt s
    putStrLn $ p1 is -- 49254779
    -- putStrLn $ p2 100 ex -- 84462026
    putStrLn $ p2 n is

p1 :: [Int] -> String
p1 = map intToDigit . take 8 . fft
p2 :: Int -> [Int] -> String
p2 n ns = 
    let offset = foldl (\acc n -> 10*acc+n) 0 $ take 7 $ ns
     -- in map intToDigit $ slice offset 8 . fft $ concat $ replicate 10000 ns
     in map intToDigit $ UVector.toList $ UVector.slice offset 8 . fftU n (length ns * 10000) . UVector.fromList $ concat $ replicate 10000 ns
 
fft :: [Int] -> [Int]
-- fft = fftL
-- fft = Vector.toList . fftV . Vector.fromList
fft l = UVector.toList . fftU 100 (length l) . UVector.fromList $ l

fftL :: [Int] -> [Int]
fftL ns = iterateN 100 step ns 
    where step !lst = zipWith use cycles lst
            where use !pattern !i = lastDigit $ sum $ zipWith (*) pattern lst
        
fftU :: Int -> Int -> UVector.Vector Int -> UVector.Vector Int
fftU !n !l = iterateN n step 
    where
    step :: UVector.Vector Int -> UVector.Vector Int
    -- step !lst = UVector.generate l $ getDigit lst
    step !lst = UVector.fromListN l ((map (getDigit lst) [0..l-1]) `using` parListChunk 100 rseq)

getDigit :: UVector.Vector Int -> Int -> Int
-- getDigit lst i = lastDigit $ UVector.sum $ UVector.zipWith (*) (getCycle (UVector.length lst) i) lst
getDigit lst = lastDigit . sumThrough . map (uncurry $ \ix sl -> UVector.sum $ UVector.slice ix sl lst) . slices (UVector.length lst) . succ

sumThrough :: [Int] -> Int
sumThrough = go 0
    where
    go !acc [] = acc
    go !acc [n] = acc+n
    go !acc (a:b:bs) = go (acc+a-b) bs

slices :: Int -> Int -> [(Int, Int)]
slices l i = 
    [ (ix,min i (l-ix))
    | ix <- [i-1,3*i-1..l-1]
    ]



cycles' :: Int -> Vector (Vector.Vector Int)
cycles' l = Vector.fromList $ take l $ map (Vector.fromList . take l) $ cycles
cyclesU :: Int -> Vector (UVector.Vector Int)
cyclesU l = Vector.generate l $ getCycle l
getCycle :: Int -> Int -> UVector.Vector Int
getCycle l i = UVector.fromList $ take l $ tail $ cycle $ dupeList (i+1) base

base = [0, 1, 0, -1]
cycles = map (tail . cycle) $ map (`dupeList` base) [1..]

dupeList :: Int -> [a] -> [a]
dupeList = foldMap . replicate
    
lastDigit :: Int -> Int
lastDigit n = abs n `mod` 10 

iterateN n f = foldr (.) id (replicate n f)

slice n o = take o . drop n

ex = map digitToInt "03036732577212944063491565474664"