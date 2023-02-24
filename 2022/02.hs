{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
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
import Data.Semigroup
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

main :: IO ()
main = do
    input <- readFile "02.txt" :: IO String
    let plays = map ((\[a,b] -> (a,b)) . map read . words) $ filter (not . null) $ lines input :: [(Play, Play)]
    print $ sum $ map (uncurry score) plays
    print $ sum $ map (uncurry score2) plays

data Play = A | B | C 
          | X | Y | Z
          deriving (Show, Read, Eq, Ord, Enum, Bounded)

score a b = win + shape 
    where 
    shape = case b of 
        X -> 1
        Y -> 2
        Z -> 3
    win = case (a,b) of 
        (A, X) -> 3
        (B, Y) -> 3
        (C, Z) -> 3
        (A, Z) -> 0
        (B, X) -> 0
        (C, Y) -> 0
        _ -> 6

score2 a b = win + shape 
    where 
    win = case b of 
        X -> 0
        Y -> 3
        Z -> 6
    shape = case b of 
        Y -> case a of 
            A -> 1
            B -> 2
            C -> 3
        Z -> case a of 
            A -> 2
            B -> 3
            C -> 1
        X -> case a of 
            A -> 3
            B -> 1
            C -> 2