{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "15"
    print $ part1 40000000 (a_value,b_value)
    print $ part2 5000000 (a_value,b_value)

move :: (Int,Int) -> (Int,Int)
move (a,b) = (movea a,moveb b)

movea,moveb :: Int -> Int
movea a = (a*16807) `rem` 2147483647
moveb b = (b*48271) `rem` 2147483647

match :: (Int,Int) -> Bool
match (a,b) = fromIntegral a == (fromIntegral b :: Word16)
    
part1 :: Int -> (Int,Int) -> Int
part1 times t = fst $ execState (replicateM_ times go) (0,t)
    where
    go = modify' $ \(!n,!s) ->
        let s' = move $! s
         in (if match s' then (n+1) else n,s')
      
part2 :: Int -> (Int,Int) -> Int
part2 times t = fst $ execState (replicateM_ times go) (0,t)
    where
    go = modify' $ \(!n,(!a,!b)) ->
        let a' = ftil movea (\x -> x `mod` 4 == 0) a
            b' = ftil moveb (\x -> x `mod` 8 == 0) b
            s' = (a',b')
         in (if match s' then (n+1) else n,s')
      
        
        
ftil :: (a -> a) -> (a -> Bool) -> a -> a
ftil f p x = let x' = f x in if p x' then x' else ftil f p x'

a_value = 116
b_value = 299