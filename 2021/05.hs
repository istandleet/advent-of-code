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
    s <- readFile "05.txt"
    input <- either fail pure $ parseOnly parseLines $ T.pack s
    -- let Input is bs = input
    print $ part1 input
    print $ part2 input

part1 = length . filter ((>=2).snd) .count . foldMap (uncurry interpolateStraight)

interpolateStraight :: Coord -> Coord -> [Coord]
interpolateStraight (x,y) (x',y')
    | x == x' = map (x,) [min y y'..max y y']
    | y == y' = map (,y) [min x x'..max x x']
    | otherwise = []

part2 = length . filter ((>=2).snd) .count . foldMap (uncurry interpolate)

interpolate :: Coord -> Coord -> [Coord]
interpolate (x,y) (x',y') = zip xs ys
    where 
    xs = mk x x' 
    ys = (if (x - x')*(y-y') >= 0 then id else reverse) $ mk y y'
    mk a b 
        | a == b = repeat a 
        | otherwise = [min a b..max a b] 

count xs = [(the x, length x) | x <- xs, then group by x using groupWith]

-- Parsing
type Coord = (Int, Int)
type Line = (Coord, Coord)

parseLines :: P.Parser [Line]
parseLines = parseLine `P.sepBy1'` "\n" <* P.skipSpace <* P.endOfInput

parseLine :: P.Parser Line
parseLine = do
    a <- P.decimal 
    P.char ','
    a' <- P.decimal 
    " -> "
    b <- P.decimal 
    P.char ','
    b' <- P.decimal 
    return $ ((a,a'),(b,b'))
