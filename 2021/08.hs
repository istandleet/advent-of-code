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
    s <- readFile "08.txt"
    print $ part1 $ parseLines s
    print $ part2 $ parseLines s

part1 = sum . map good 
    where 
    good (hints,word) = 
        let easy = solve1 hints
         in length $ filter (\w -> Set.fromList w `elem` easy) word

part2 :: [Line] -> Int
part2 = sum .part2'
part2' = map (uncurry go)
    where
    go hints ss =  fromdigits $ (\p -> map (decode p) ss) $ solve2 hints

solve1 :: [String] -> [Set Char]
solve1 hints = [solveEasy i hints | i <- [0..9]]

solveEasy :: Int -> [String] -> Set Char
solveEasy 1 hints = Set.fromList $ head $ filter ((==2) . length) hints
solveEasy 4 hints = Set.fromList $ head $ filter ((==4) . length) hints
solveEasy 7 hints = Set.fromList $ head $ filter ((==3) . length) hints
solveEasy 8 hints = Set.fromList $ head $ filter ((==7) . length) hints
solveEasy n hints = mempty

decode :: String -> String -> Int
decode codex word = (m Map.!)
    $ Set.fromList
    $ map fst
    $ filter ((`Set.member` Set.fromList word) . snd) 
    $ zip [0..] codex
    
m :: Map (Set Int) Int
m = Map.fromList
    [(Set.fromList [0,1,2,  4,5,6], 0)
    ,(Set.fromList [    2,    5  ], 1)
    ,(Set.fromList [0,  2,3,4,  6], 2)
    ,(Set.fromList [0,  2,3,  5,6], 3)
    ,(Set.fromList [  1,2,3,  5  ], 4)
    ,(Set.fromList [0,1,  3,  5,6], 5)
    ,(Set.fromList [0,1,  3,4,5,6], 6)
    ,(Set.fromList [0,  2,    5  ], 7)
    ,(Set.fromList [0,1,2,3,4,5,6], 8)
    ,(Set.fromList [0,1,2,3,  5,6], 9)
    ]

-- solve2 :: [String] -> [Char]
solve2 hints = zipWith go ['a'..] $ solveH hints
    where 
    one = solveEasy 1 hints
    go 'c' opts = head $ filter (`Set.member` one) opts
    go 'a' opts = head $ filter (`Set.notMember` one) opts
    go 'd' opts = let cs = solveEasy 4 hints in head $ filter (`Set.member` cs) opts
    go 'g' opts = let cs = solveEasy 4 hints in head $ filter (`Set.notMember` cs) opts
    go c opts 
        | length opts == 1 = head opts
        | otherwise = '-'

solveH hints = [solveHard i hints | i <- ['a'..'g']]

solveHard :: Char -> [String] -> [Char]
solveHard n hints = fmap fst . filter ((==c) . snd) $ count $ concat hints
    where c = occurs Map.! n

occurs :: Map Char Int
occurs = Map.fromList
    [('a', 8)
    ,('b', 6)
    ,('c', 8)
    ,('d', 7)
    ,('e', 4)
    ,('f', 9)
    ,('g', 7)
    ]


count :: Ord a => [a] -> [(a,Int)]
count xs = [(the x, length x) | x <- xs, then group by x using groupWith]

fromdigits :: Num a => [a] -> a
fromdigits = foldr (\a b -> 10*b + a) 0 . reverse

-- Parsing
type Coord = (Int, Int)
type Line = ([String],[String])

parseLines :: String -> [Line]
parseLines = map parseLine . filter (not . null) . lines

parseLine :: String -> Line
parseLine s = 
    let w = words s
     in (take 10 w, take 4 $ drop 11 w)