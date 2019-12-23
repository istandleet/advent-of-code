{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Data.List
import Data.Array.Unboxed 

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Primes


main = do
    putStrLn "6"
    let (n,rep) = part1 s
    print n
    print $ fst $ part1 rep
    
part1 :: Vector Int -> (Int,Vector Int)
part1 v = go 0 (Set.singleton v) v
    where
    go !n !acc !curr = let nxt = p1step curr in if nxt `Set.member` acc then (n+1,nxt) else go (n+1) (Set.insert nxt acc) nxt
    
p1step :: Vector Int -> Vector Int
p1step v =
    let Just i = Vector.findIndex (== Vector.maximum v) v
     in Vector.modify (p1Modify i $ Vector.length v) v
     
-- p1Modify :: Int -> Vector Int -> Vector Int
p1Modify i l v = do
    n <- MVector.read v i
    MVector.write v i 0
    go n (nxt i)
    where
    nxt ix = (ix+1) `mod` l
    go 0 _  = return ()
    go n ix = MVector.modify v succ ix >> go (pred n) (nxt ix)
    
s :: Vector Int
s = Vector.fromList [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]
ex :: Vector Int
ex = Vector.fromList [0,2,7,0]