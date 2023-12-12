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
    input <- readInput <$> readFile "07.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 = sum . zipWith (*) [1..] . map snd . Data.List.sortOn (value . fst)
    where value hand = (assignHandType hand, map cardRank hand)
part2 = sum . zipWith (*) [1..] . map snd . Data.List.sortOn (value . fst)
    where value hand = (assignHandType' hand, map cardRank' hand)

data HandType
   = HighCard
   | OnePair
   | TwoPair
   | ThreeOfAKind
   | FullHouse
   | FourOfAKind
   | FiveOfAKind
    deriving (Show, Eq, Ord, Enum, Bounded)

assignHandType :: Hand -> HandType
assignHandType = go . Data.List.sortOn (negate . length) . Data.List.group . Data.List.sort
    where
    go ls = case ls of
        [_] -> FiveOfAKind
        [a,b] -> if length a == 4 then FourOfAKind else FullHouse
        [a,b,c] -> if length a == 3 then ThreeOfAKind else TwoPair
        [a,b,c,d] -> OnePair
        _ -> HighCard

cardRank :: Char -> Int
cardRank c = fromMaybe 0 $ lookup c $ zip "23456789TJQKA" [2..]


assignHandType' :: Hand -> HandType
assignHandType' hand = go . Data.List.sortOn (negate . length) . Data.List.group 
                     . Data.List.sort $ filter (/='J') hand
    where
    js = length $ filter (=='J') hand
    go ls = case ls of
        [] -> FiveOfAKind -- all Js
        [_] -> FiveOfAKind
        [a,b] -> if (length a+js) == 4 then FourOfAKind else FullHouse
        [a,b,c] -> if (length a+js) == 3 then ThreeOfAKind else TwoPair
        [a,b,c,d] -> OnePair
        _ -> HighCard

cardRank' :: Char -> Int
cardRank' c = fromMaybe 0 $ lookup c $ zip "J23456789TQKA" [2..]

type Hand = String

type Input = [(Hand, Int)]
readInput :: String -> Input
readInput = map go . filter (not . null) . lines
    where go s = case words s of [a,b] -> (a, read b)
