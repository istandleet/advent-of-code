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
    input <- readFile "02.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1, part2 :: Input -> Int
part1 = sum . map fst . filter (all pass1 . snd)

pass1 :: [Cube] -> Bool
pass1 = all pass
    where
    pass (Cube n s) = s `elem` ["red", "green", "blue"]
            && case s of 
                "red" -> n <= 12
                "green" -> n <= 13
                "blue" -> n <= 14

part2 = sum . map product . map covering . map snd

covering :: [[Cube]] -> Map Text Int
covering = Map.unionsWith max . map (Map.fromList . map (\(Cube n s) -> (s, n)))

type Input = [Line]
parseInput :: P.Parser Input
parseInput = sepBy1' parseLine endOfLine <* skipSpace <* endOfInput

data Cube = Cube Int Text deriving (Show, Eq, Ord)
type Line = (Int, [[Cube]])

parseLines :: P.Parser [Line]
parseLines = parseLine `P.sepBy1'` "\n" <* P.skipSpace <* P.endOfInput

parseLine :: P.Parser Line
parseLine = do
    "Game "
    n <- decimal
    ": "
    cubes <- parseCubes `sepBy1'` "; "
    return (n, cubes)

parseCubes :: P.Parser [Cube]
parseCubes = parseCube `sepBy1'` ", "

parseCube :: P.Parser Cube
parseCube = do
    n <- decimal
    " "
    s <- P.takeWhile1 isAlpha
    return (Cube n s)
