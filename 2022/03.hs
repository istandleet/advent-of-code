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
    input <- readFile "03.txt" :: IO String
    let plays = filter (not . null) $ lines input
    print $ sum $ map (score . spread) plays
    print $ sum $ map score $ part2 plays

score :: Char -> Int
score c = if isAsciiLower c then fromEnum c - fromEnum 'a' + 1 else fromEnum c - fromEnum 'A' + 27

spread :: String -> Char
spread l = 
    let (a, b) = splitAt (length l `div` 2) l 
     in minimum $ Set.fromList a `Set.intersection` Set.fromList b 

part2 :: [String] -> [Char]
part2 (a:b:c:es) = go a b c : part2 es
    where
        go a b c = minimum $ Set.fromList a `Set.intersection` Set.fromList b `Set.intersection` Set.fromList c
part2 _ = []