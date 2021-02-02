{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

main :: IO ()
main = do
    s <- readFile "09.txt"
    let input = map read $ filter (not.null) $ lines s
    let mp1 = part1 25 input
    case mp1 of
        Nothing -> fail "failed at part1, can't do p2"
        Just p1 -> do
            print p1
            let mp2 = part2 p1 input
            case mp2 of
                Nothing -> fail "failed at part2"
                Just p2 -> do
                    print p2
                    print (minimum p2 + maximum p2)

part1 :: Int -> [Int] -> Maybe Int
part1 preamble = uncurry go . Seq.splitAt preamble . Seq.fromList
    where
    go :: Seq Int -> Seq Int -> Maybe Int
    go a Seq.Empty = Nothing
    go a@(_ Seq.:<| rest) (nxt Seq.:<| b) = if satisfied a nxt
        then go (rest Seq.:|> nxt) b
        else Just nxt

satisfied :: Seq Int -> Int -> Bool
satisfied Seq.Empty _ = False
satisfied (nxt Seq.:<| b) n = case Seq.elemIndexL (n-nxt) b of
    Nothing -> satisfied b n
    Just _ -> True

part2 :: Int -> [Int] -> Maybe (Seq Int)
part2 goal = go mempty . Seq.fromList
    where
    -- go :: Seq Int -> Seq Int -> Maybe (Seq Int)
    go as bs = case compare (sum as) goal of
        EQ -> Just as
        LT -> case bs of 
            nxt Seq.:<| rest -> go (as Seq.:|> nxt) rest
            Seq.Empty -> Nothing
        GT -> case as of
            _ Seq.:<| rest -> go rest bs
            Seq.Empty -> error "empty GT goal"
        