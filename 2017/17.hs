{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Foldable
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (the, groupWith)
import Data.Matrix hiding ((<|>))

import Data.Char
import Data.Attoparsec.Text hiding (D, count)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "17"
    let n = 348
    print $ part1 n
    print $ part2 n
    
part1 n = maybe (-1) (Seq.index s . succ) $ Seq.elemIndexL 2017 s
    where s = steps n 2017
    
part2 n = afterZero n 50000000
    
steps :: Int -> Int -> Seq Int
steps n num = go 1 0 $ Seq.singleton 0 
    where
    go !i !curr !s 
        | i > num   = s
        | otherwise = uncurry (go $ i+1) $ step n i curr s
        
afterZero :: Int -> Int -> Int
afterZero n num = go 1 0 1
    where
    go !i !curr !s 
        | i > num   = s
        | otherwise = 
            let curr' = step' n i curr 
             in go (i+1) curr' (if curr' == 1 then i else s)

step :: Int -> Int -> Int -> Seq Int -> (Int, Seq Int)
step n i curr s = 
    let curr' = ((n + curr) `mod` length s) + 1
     in (curr', Seq.insertAt curr' i s)
     
step' :: Int -> Int -> Int -> Int
step' n i curr = ((n + curr) `mod` i) + 1