{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    print $ part1 1965712 19072108 -- 16881444

part1 :: Int -> Int -> Int
part1 a b = case Data.List.findIndex (==a) (stream 7) of
    Nothing -> error "lol"
    Just at -> stream b !! at 

step :: Int -> Int -> Int
step sn v = (sn*v) `mod` _MOD

stream :: Int -> [Int]
stream sn = tail $ iterate (step sn) 1

_MOD = 20201227

{-

part1 :: Int -> Int -> Int
part1 a b = (\(_,_,c) -> c) $ head $ filter (isAnswer a b) $ findCrosses a b

isAnswer :: Int -> Int -> (Int,Int,Int) -> Bool
isAnswer a b (at,bt,_) = loop bt == fromIntegral a && loop at == fromIntegral b

loop :: Int -> Mod 20201227
loop = powMod 7

findCrosses :: Int -> Int -> [(Int,Int,Int)]
findCrosses a b = findCrosses' 1 mempty (stream a) (stream b)
    
findCrosses' :: Int -> (IntMap Int, IntMap Int) -> [Int] -> [Int] -> [(Int,Int,Int)] 
findCrosses' i _ _ _ | i >= _MOD = []
findCrosses' i (as,bs) (x:xs) (y:ys) = f rest
    where
    rest = findCrosses' (succ i) (insert_keep x i as, insert_keep y i bs) xs ys
    insert_keep = IntMap.insertWith (flip const)
    acase = case IntMap.lookup x bs of 
        Nothing -> Nothing
        Just yix -> Just (i,yix,x)
    bcase = case IntMap.lookup y as of 
        Nothing -> Nothing
        Just xix -> Just (xix,i,y)
    abcase = if isJust acase || isJust bcase || x /= y then Nothing else Just (i,i,x)
    f = foldr (\t f -> (t:) . f) id $ catMaybes [acase,bcase,abcase]

-}