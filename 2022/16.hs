{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Semigroup
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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take, I)
import qualified Data.Attoparsec.Text as P

import Numeric.LinearAlgebra as M

main :: IO ()
main = do
    input <- readFile "16.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1 :: Input -> Int
part1 input = 
    let dists = distances 30 input
        ss = mkscores input
     in maximum $ map (evalPath 30 dists ss) $ paths 30 dists
part2 input = 
    let dists = distances 26 input
        ss = mkscores input
     in part2Fin
      $ dropDuplicatesOn fst
      $ map (first Set.fromList) 
      $ reverse $ Data.List.sortOn snd 
      $ map ((,) <$> id <*> evalPath 26 dists ss) $ allPaths 26 dists

part2Fin :: [(SPath, Int)] -> ((SPath,SPath), Int)
part2Fin (p:ps) = go (highestPairing p ps) ps 
    where
    go !acc (a:rest) = 
        if snd acc >= sum (map snd $ take 2 rest)
            then acc
            else go (maxOn snd acc $ highestPairing a rest) rest
    go !acc _ = acc

highestPairing :: (SPath, Int) -> [(SPath, Int)] -> ((SPath, SPath), Int)
highestPairing (p, s1) = maybe ((p,mempty),s1) (\(p1,s2) -> ((p,p1),s1+s2)) 
                       . listToMaybe 
                       . dropWhile (not . Set.disjoint p . fst)

type ValveName = Text
type Valve = (ValveName, Int, [ValveName])
type Path = [ValveName]
type SPath = Set ValveName
type Input = [Valve]

mkscores :: [(ValveName, Int, [ValveName])] -> Map ValveName Int
mkscores = Map.fromList . map (\(a,b,_) -> (a,b))

buildAdjacency :: [(ValveName, [ValveName])] -> (Map ValveName Int, Matrix R)
buildAdjacency valves = (decoder, mat)
    where
    decoder = Map.fromList $ zip (map fst valves) [0..]
    mat = matrix (length valves)
        [ if v `elem` conns then 1 else 0
        | (name, conns) <- valves
        , (i, v) <- zip [0..] $ map fst valves
        ]

type Distances = Map ValveName (Map ValveName Int)
-- | filters to important valves (AA + those with pressure)
distances :: Int -> [(ValveName, Int, [ValveName])] -> Distances
distances n input = Map.fromList 
    [ (a, Map.fromList [(b, dist a b)| b <- important, a /= b]) 
    | a <- Set.toList $ Set.fromList $ "AA" : important
    ]
    where
    (decoder, mat) = buildAdjacency $ map (\(a,_,c) -> (a,c)) input
    mats = take n $ iterate (mat M.<>) mat
    important = map (\(a,_,_) -> a) (filter (\(_,b,_) -> b > 0) input)
    dist a b = 
        let i = decoder Map.! a
            j = decoder Map.! b
         in maybe (succ n) succ $ Data.List.findIndex (\m -> M.atIndex m (i,j) > 0.5) $ mats

paths :: Int -> Distances -> [Path]
paths n dists = go "AA" mempty 0
    where
    important = dists Map.! "AA"
    go :: ValveName -> Set ValveName -> Int -> [Path]
    go a seen d | length seen == length important = [[]]
    go a seen d = 
        [ ps
        | (b, d') <- Map.toList $ dists Map.! a
        , b `Set.notMember` seen
        , let tot = d + d' + 1 -- +1 to open valve
        , ps <- if tot > n then [[]] else map (b:) $ go b (Set.insert b seen) tot
        ]

allPaths :: Int -> Distances -> [Path]
allPaths n dists = go "AA" mempty 0
    where
    important = dists Map.! "AA"
    go :: ValveName -> Set ValveName -> Int -> [Path]
    go a seen d | length seen == length important = [[]]
    go a seen d = []:
        [ ps
        | (b, d') <- Map.toList $ dists Map.! a
        , b `Set.notMember` seen
        , let tot = d + d' + 1 -- +1 to open valve
        , ps <- if tot > n then [[]] else map (b:) $ go b (Set.insert b seen) tot
        ]

evalPath :: Int -> Distances -> Map ValveName Int -> Path -> Int
evalPath n ds ss = go n . ("AA":)
    where
    go _ [] = 0
    go _ [_] = 0
    go t (a:b:ps) =
        let d = ds Map.! a Map.! b
            s = ss Map.! b
            t' = t - d - 1
         in t'*s + go t' (b:ps)

-- * Util
maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f a b = if f a >= f b then a else b

dropDuplicatesOn :: Ord b => (a -> b) -> [a] -> [a]
dropDuplicatesOn f = go mempty
    where
    go _ [] = []
    go seen (a:as) = 
        let b = f a
         in if b `Set.member` seen
                then go seen as
                else a : go (Set.insert b seen) as

-- * Parsing
parseInput :: Parser Input
parseInput = sepBy1' parseLine endOfLine <* skipSpace <* endOfInput

parseLine :: Parser Valve
parseLine = do
    "Valve "
    name <- P.take 2
    " has flow rate="
    rate <- decimal
    "; tunnels lead to valves " <|> "; tunnel leads to valve "
    valves <- sepBy1' (P.take 2) (string ", ")
    return (name, rate, valves)

