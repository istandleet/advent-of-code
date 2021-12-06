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
    s <- readFile "04.txt"
    input <- either fail pure $ parseOnly parseInput $ T.pack s
    -- let Input is bs = input
    print $ part1 input
    print $ part2 input

part1 (Input is bs) = 
    let Just i = find (\is -> any (hasBingo (Set.fromList is)) bs) $ Data.List.inits is
        Just b = find (hasBingo $ Set.fromList i) bs
     in last i * notmarked (Set.fromList i) b
    
part2 (Input is bs) = 
    let Just i = find (\is -> all (hasBingo (Set.fromList is)) bs) $ Data.List.inits is
        Just b = find (not . hasBingo (Set.fromList $ init i)) bs
     in last i * notmarked (Set.fromList i) b

notmarked is bs = sum
    [ i
    | (y,l) <- zip [0..] bs
    , (x,i) <- zip [0..] l 
    , i `Set.notMember` is
    ]

hasBingo :: Set Int -> Board -> Bool
hasBingo is bs = isBingo $ mark is bs

mark :: Set Int -> Board -> Set Coord
mark is bs = Set.fromList
    [ (x,y)
    | (y,l) <- zip [0..] bs
    , (x,i) <- zip [0..] l 
    , i `Set.member` is
    ]

isBingo :: Set Coord -> Bool
isBingo is = any (`Set.isSubsetOf` is) bingos

bingos :: [Set Coord]
bingos = map Set.fromList 
       $ map (\x -> map (x,) [0..4]) [0..4] <> map (\y -> map (,y) [0..4]) [0..4]

type Coord = (Int, Int)

-- Parsing
data Input = Input [Int] [Board] deriving Show
type Board = [[Int]]

parseInput :: P.Parser Input
parseInput = do
    a <- P.decimal `sepBy1` P.char ','
    P.skipSpace 
    b <- parseBoard `sepBy1` (P.skipSpace)
    P.skipSpace 
    P.endOfInput
    return $ Input a b

parseBoard :: P.Parser Board
parseBoard = ((many' (P.char ' ') *> P.decimal) `sepBy1` P.char ' ') `sepBy1` P.endOfLine
