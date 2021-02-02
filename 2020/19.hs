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

main :: IO ()
main = do
    s <- readFile "19x.txt"
    input <- either fail pure $ parseOnly parseInput $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 input = length $ filter good $ codes input
    where
    rulemap = Map.fromList $ rules input
    parser = toParser rulemap 0 <* P.endOfInput
    good = isRight . parseOnly parser

part2 :: Input -> Int
part2 input = length $ filter good $ codes input
    where
    -- 8: 42 | 42 8
    -- 11: 42 31 | 42 11 31
    hardcode = Map.insert 8 (Choice [42] [42,8]) . Map.insert 11 (Choice [42,31] [42,11,31])
    rulemap = hardcode $ Map.fromList $ rules input
    parser = toParser rulemap 0 <* P.endOfInput
    good = isRight . parseOnly parser

part2' input = filter good $ codes input
    where
    -- 8: 42 | 42 8
    -- 11: 42 31 | 42 11 31
    hardcode = Map.insert 8 (Choice [42] [42,8]) . Map.insert 11 (Choice [42,31] [42,11,31])
    rulemap = hardcode $ Map.fromList $ rules input
    parser = toParser rulemap 0 <* P.endOfInput
    good = isRight . parseOnly parser

data Input = Input
   { rules :: [(Int, Rule)]
   , codes :: [Text]
   } deriving (Show, Eq)

data Rule =
     Exact Text
   | Sequence [Int]
   | Choice [Int] [Int]
   deriving (Show, Eq)

toParser :: Map Int Rule -> Int -> P.Parser ()
toParser m = go
    where 
    go i = case m Map.! i of 
        Exact str -> () <$ P.string str 
        Sequence is -> fromSeq is
        Choice is is' -> fromSeq is <|> fromSeq is'

    fromSeq = mapM_ go

toParserTrace :: Map Int Rule -> Int -> P.Parser [Int]
toParserTrace m = go
    where 
    go i = (<?> show i) $ fmap (i:) $ case m Map.! i of 
        Exact str -> [] <$ P.string str 
        Sequence is -> fromSeq is
        Choice is is' -> fromSeq is <|> fromSeq is'

    fromSeq = fmap concat . mapM go

-- simplifyRules :: Map Int Rule -> Map Int Rule
simplifyRules m = filter (isKnown m) (Map.keys m)
    where
    isKnown m i = case m Map.! i of
        Exact _ -> True
        Sequence is -> all noncyclic is
        Choice a b -> all noncyclic a && all noncyclic b
        where noncyclic i' = i /= i' && isKnown m i'

-- Parsing
parseInput :: P.Parser Input
parseInput = do
    rules <- parseRule `sepBy1` P.endOfLine
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
        str <- P.takeWhile1 (\c -> c /= '"')
        char '"'
        return $ Exact str
    parseChoice = do
        a <- parseSeq
        " | "
        b <- parseSeq
        return $ Choice a b
    parseSeq = P.decimal `sepBy1` P.char ' '
