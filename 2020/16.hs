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

main :: IO ()
main = do
    s <- readFile "16.txt"
    input <- either fail pure $ parseOnly parseInput $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: Input -> Integer
part1 input = sum $ map sum 
            $ map (V.filter (\n -> not $ any (`passesRange` n) $ ranges input)) 
            $ tickets input

part2 :: Input -> Integer
part2 Input{..} = product $ map (ticket V.!) interesting
    where
    validTickets = filter (all (\n -> any (`passesRange` n) ranges)) tickets
    order = rangeOrder ranges validTickets
    interesting = map fst $ filter (T.isPrefixOf "departure" . label . snd) $ zip [0..] order

part2' Input{..} = filter (T.isPrefixOf "departure" . label . snd) $ zip [0..] $ rangeOrder ranges $ filter (all (\n -> any (`passesRange` n) ranges)) tickets

rangeOrder :: [Range] -> [Ticket] -> [Range]
rangeOrder ranges validTickets = map (ranges!!) $ map snd $ Data.List.sort $ map swap reordering 
    where reordering = solvedIndices $ trivialSimplification $ rangeMatrix ranges validTickets

rangeMatrix :: [Range] -> [Ticket] -> Array (Int, Int) Bool
rangeMatrix ranges validTickets = array bnds
    [ ((i,j), all (\t -> passesRange r (t V.! j)) validTickets)
    | (i,r) <- zip [0..] ranges
    , j <- [0..ticketindex]
    ]
    where
    ticketindex = length ranges-1
    bnds = ((0,0),(ticketindex, ticketindex))

trivialSimplification :: Array (Int, Int) Bool -> Array (Int, Int) Bool
trivialSimplification = fst . head . dropWhile (uncurry (/=)) . (\a -> zip a (tail a)) . iterate trivialSimplification'

-- | for an ixj matrix does sudoku like simplification
trivialSimplification' :: Array (Int, Int) Bool -> Array (Int, Int) Bool
trivialSimplification' arr = array (bounds arr)
    [ (ix, arr!ix && not (ix `Set.member` known_wrong))
    | ix <- range $ bounds arr
    ]
    where 
    ((xmin,ymin),(xmax,ymax)) = bounds arr
    related (x,y) = map (,y) [xmin..xmax] ++ map (x,) [ymin..ymax]
    solved = Set.fromList $ solvedIndices arr
    known_wrong = foldMap (Set.fromList . related) solved Set.\\ solved

solvedIndices :: Array (Int, Int) Bool -> [(Int, Int)]
solvedIndices arr = filter solved $ range $ bounds arr
    where 
    ((xmin,ymin),(xmax,ymax)) = bounds arr
    solved ix@(x,y) = arr ! ix 
             && ( all (\ix' -> ix == ix' || not (arr!ix')) (map (,y) [xmin..xmax])
               || all (\ix' -> ix == ix' || not (arr!ix')) (map (x,) [ymin..ymax])
                )


type Ticket = V.Vector Integer

data Input = Input
   { ranges :: [Range]
   , ticket :: Ticket
   , tickets :: [Ticket]
   } deriving (Show, Eq)

data Range = Range
   { label :: Text
   , option1 :: (Integer,Integer)
   , option2 :: (Integer,Integer)
   } deriving (Show, Eq)

passesRange :: Range -> Integer -> Bool
passesRange (Range _ (a,a') (b,b')) n = (a <= n && n <= a') || (b <= n && n <= b')

-- Parsing
parseInput :: P.Parser Input
parseInput = do
    ranges <- parseRange `sepBy1` P.endOfLine
    P.endOfLine
    P.endOfLine
    "your ticket:"
    P.endOfLine
    ticket <- parseTicket
    P.endOfLine
    P.endOfLine
    "nearby tickets:"
    P.endOfLine
    tickets <- parseTicket `sepBy1` P.endOfLine
    P.endOfLine
    P.endOfInput
    return Input{..}

-- 13-40 or 45-50
parseRange :: P.Parser Range
parseRange = do
    label <- P.takeWhile1 (/=':')
    ": "
    a <- decimal
    P.char '-'
    a' <- decimal
    " or "
    b <- decimal
    P.char '-'
    b' <- decimal
    return $ Range label (a,a') (b,b')

parseTicket :: P.Parser Ticket
parseTicket = fmap V.fromList $ decimal `sepBy1` char ','
