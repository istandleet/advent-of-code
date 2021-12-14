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
import Data.Attoparsec.Text as P hiding (takeWhile, take, count)

main :: IO ()
main = do
    input <- filter (not.null) . lines <$> readFile "10.txt"
    print $ part1 input
    print $ part2 input

part1, part2 :: [String] -> Integer
part1 = sum . map go1
part2 = median . map (score2 . go2) . filter ((==0) . go1)

go1 = maybe 0 value . listToMaybe . filter (isNothing . pair) . stablePoint . iterate step
    where
    value = \case 
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137

go2 = mapMaybe pair . reverse . stablePoint . iterate step

score2 = fromdigits . map value2
    where
value2 = \case 
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
fromdigits = foldl (\a b -> 5*a + b) 0

step [] = []
step [c] = [c]
step (c:c':cs) = case pair c of
    Just tar | tar == c' -> step cs
    _ -> c : step (c':cs)

pair c = case c of 
    '[' -> Just ']'
    '{' -> Just '}'
    '(' -> Just ')'
    '<' -> Just '>'
    _ -> Nothing

stablePoint :: Eq a => [a] -> a
stablePoint ls = fst $ head $ filter (uncurry (==)) $ zip ls (tail ls)

median l = Data.List.sort l !! (length l `div` 2)