{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
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
    input <- readFile "07.txt"
    let file = parseInput input
    print $ part1 file
    print $ part2 70000000 file

part1 :: File Int -> Int
part1 f = case f of
    Dir _ fs -> (if dirsize f <= 100000 then dirsize f else 0) + sum (map part1 fs)
    File _ s -> 0

part2 :: Int -> File Int -> Int
part2 space f = minimum $ filter (>= todel)  (go f)
    where 
    remaining = space - dirsize f
    todel = 30000000 - remaining
    go file = case file of 
        Dir _ fs -> dirsize file : foldMap go fs
        File _ _ -> []

dirsize = \case
    Dir _ fs -> sum $ map dirsize fs
    File _ s -> s

data File a = Dir String [File a] | File String a deriving (Show, Eq, Functor, Foldable)

parseInput :: String -> File Int
parseInput = head . snd . parseDir . filter (not . null) . lines

parseDir :: [String] -> ([String], [File Int])
parseDir (c:cs) = case words c of 
    ["$", "cd", ".."] -> (cs, [])
    ["$", "cd", dir] -> 
        let (cs', fs) = parseDir cs
         in fmap (Dir dir fs :) $ parseDir cs'
    ["$", "ls"] -> 
        let (out, rest) = break ((=='$') . head) cs
            (cs', fs) = parseDir rest
         in (cs', parseFiles out ++ fs)
parseDir [] = ([], [])

parseFiles :: [String] -> [File Int]
parseFiles = mapMaybe go
    where 
    go l = case words l of
        ["dir", name] -> Nothing
        [size, name] -> Just $ File name (read size)
