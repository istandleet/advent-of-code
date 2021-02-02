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
    s <- readFile "24.txt"
    input <- either fail pure $ mapM (parseOnly parseLine) $ map T.pack $ filter (not.null) $ lines s
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Int
part1 = length . initalSet

initalSet :: [Line] -> Set Coord
initalSet = Set.fromList . map fst . filter (odd . snd) . count . map (foldMap onestep)

part2 :: [Line] -> Int
part2 = length . (!! 100) . iterate step . initalSet

step :: Set Coord -> Set Coord
step blacks = Set.filter shouldBeBlack possible
    where
    possible = blacks <> foldMap (\ix -> Set.fromList $ map (`move` ix) [minBound..maxBound]) blacks
    shouldBeBlack ix = blackNeighbors == 2 || (ix `Set.member` blacks && blackNeighbors == 1)
        where blackNeighbors = length $ filter (`Set.member` blacks) $ map (`move` ix) [minBound..maxBound]

data Dir = E | NE | SE | W | SW | NW deriving (Eq,Show,Ord,Enum,Bounded)

data Coord = Coord
    { _east     :: {-# unpack #-} !Int
    , _northwest :: {-# unpack #-} !Int
    } deriving (Show, Eq, Ord)

instance Semigroup Coord where Coord a b <> Coord a' b' = Coord (a + a') (b + b')
instance Monoid Coord where mempty = Coord 0 0

onestep :: Dir -> Coord
onestep d = move d mempty
move :: Dir -> Coord -> Coord
move E  = east %~ succ
move W  = east %~ pred
move NW = northwest %~ succ
move SE = northwest %~ pred
move NE = move NW . move E
move SW = move SE . move W

-- * Util
count xs = [(the x, length x) | x <- xs, then group by x using groupWith]

-- * Parsing
type Line = [Dir]

parseLine :: P.Parser Line
parseLine = many1' parseDir <* endOfInput

parseDir :: P.Parser Dir
parseDir = NE <$ "ne"
       <|> NW <$ "nw"
       <|> E  <$ "e"
       <|> SE <$ "se"
       <|> SW <$ "sw"
       <|> W  <$ "w"

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Coord
east :: Lens' Coord Int
east f_amr2 (Coord x1_amr3 x2_amr4)
    = (fmap (\ y1_amr5 -> (Coord y1_amr5) x2_amr4)) (f_amr2 x1_amr3)
{-# INLINE east #-}
northwest :: Lens' Coord Int
northwest f_amr6 (Coord x1_amr7 x2_amr8)
    = (fmap (\ y1_amr9 -> (Coord x1_amr7) y1_amr9)) (f_amr6 x2_amr8)
{-# INLINE northwest #-}
