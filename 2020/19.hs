{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

-- issue: `isLeft $ parseOnly (("a"<|>"ab")>>"b">>endOfInput) "abb"`
hard = Input rules ["abb"]
    where 
    rules = Map.fromList [
        (1,Exact 'a'),
        (2,Exact 'b'),
        (3,Choice [1] [1,2]),
        -- (3,flip Choice [1] [1,2]),
        (0,Sequence [3,2])
        ]

main :: IO ()
main = do
    s <- readFile "19.txt"
    input <- either fail pure $ parseOnly parseInput $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 input = length $ filter good $ codes input
    where good = any null . apply (rules input) 0 . pure . T.unpack

part2 :: Input -> Int
part2 (Input a b) = part1 (Input (hardcode a) b)

-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31
hardcode :: Map Int Rule -> Map Int Rule
hardcode = Map.insert 8 (Choice [42] [42,8]) . Map.insert 11 (Choice [42,31] [42,11,31])

data Input = Input
   { rules :: Map Int Rule
   , codes :: [Text]
   } deriving (Show, Eq)

data Rule =
     Exact Char
   | Sequence [Int]
   | Choice [Int] [Int]
   deriving (Show, Eq)

apply :: Map Int Rule -> Int -> [String] -> [String]
apply rules int = case rules Map.! int of 
    Exact c -> mapMaybe $ \case 
                            (c':cs) | c' == c -> Just cs
                            _ -> Nothing
    Sequence is -> go is
    Choice is js -> go is <> go js
    where
    go :: [Int] -> [String] -> [String]
    go [] ss = ss
    go _ [] = []
    go (i:is) ss = go is $ apply rules i ss

-- Parsing
parseInput :: P.Parser Input
parseInput = do
    rules <- fmap Map.fromList $ parseRule `sepBy1` P.endOfLine
    P.endOfLine
    P.endOfLine
    codes <- P.takeWhile1 (\c -> not $ c == '\r' || c == '\n') `sepBy1` P.endOfLine
    P.endOfLine
    P.endOfInput
    return Input{..}

parseRule :: P.Parser (Int, Rule)
parseRule = do
    num <- decimal
    ": "
    rule <- parseLit <|> parseChoice <|> (Sequence <$> parseSeq)
    return (num, rule)
    where
    parseLit = do
        char '"'
        str <- P.anyChar
        char '"'
        return $ Exact str
    parseChoice = do
        a <- parseSeq
        " | "
        b <- parseSeq
        return $ Choice a b
    parseSeq = P.decimal `sepBy1` P.char ' '
