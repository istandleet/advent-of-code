{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Lens

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Time
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, D)

main = do
    input <- readFile "01.txt"
    print $ part1 $ readInput input
    print $ part2 $ readInput' input

part1, part2 :: Input -> Int
part1 = sum
part2 = sum

type Input = [Int]
readInput = map (readInt . filter isDigit) . filter (not . null) . lines

readInt ls = read [head ls, last ls]

readInput' = map readInt'' . filter (not . null) . lines

readInt' :: String -> Int
readInt' = fin . go
    where
    go (a:xs) = if isDigit a then read [a] : go xs 
            else case readDigit (a:xs) of
                    Just (n, xs') -> n : go xs'
                    Nothing -> go xs
    go [] = []
    fin ns = 10 * head ns + last ns

readInt'' :: String -> Int
readInt'' = fin . mapMaybe go . Data.List.tails
    where
    go (a:xs) = if isDigit a then Just (read [a])
            else fst <$> readDigit (a:xs)
    go [] = Nothing
    fin ns = 10 * head ns + last ns

readDigit ('o':'n':'e':xs) = Just (1, xs)
readDigit ('t':'w':'o':xs) = Just (2, xs)
readDigit ('t':'h':'r':'e':'e':xs) = Just (3, xs)
readDigit ('f':'o':'u':'r':xs) = Just (4, xs)
readDigit ('f':'i':'v':'e':xs) = Just (5, xs)
readDigit ('s':'i':'x':xs) = Just (6, xs)
readDigit ('s':'e':'v':'e':'n':xs) = Just (7, xs)
readDigit ('e':'i':'g':'h':'t':xs) = Just (8, xs)
readDigit ('n':'i':'n':'e':xs) = Just (9, xs)
readDigit _ = Nothing
