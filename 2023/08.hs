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
import qualified Data.Attoparsec.Text as P

main = do
    input <- readFile "08.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input -- 3717755504527 too low, 11012749988362659073991217 too high

part1, part2 :: Input -> Int
part1 (dirs, routes) = go 0 "AAA" $ cycle dirs
    where
    m = Map.fromList routes
    go !acc "ZZZ" _ = acc
    go !acc !t (dir:dirs) = go (acc + 1) (step m dir t) dirs
part2 (dirs, routes) = go $ map (zIndices m dirs) as
    where
    m = Map.fromList routes
    as = filter ((=='A') . T.last) $ map fst routes
    go :: [ZIndicies] -> Int
    go zindexes =
        let p = the [x | (_,x,_,_) <- zindexes]
            zs = [(fromIntegral q,fromIntegral mo) | ([],_,[r],mo) <- zindexes, let (q, 0) = r `divMod` length dirs]
            c = crt zs
         in (fromInteger c+p) * (length dirs)

type RouteMap = Map Text (Text, Text)

findPeriodicity :: RouteMap -> [Bool] -> Text -> ([Text], Int)
findPeriodicity m dirs t = go mempty t
    where
    go !seen t
        | t `elem` seen = let (rep, ini) = break (==t) seen in (t:reverse rep, length $ tail ini)
        | otherwise = go (t:seen) $ foldl (flip $ step m) t dirs

type ZIndicies = ([Int], Int, [Int], Int)
zIndices :: RouteMap -> [Bool] -> Text -> ZIndicies
zIndices m dirs t = 
    let (cyc, p) = findPeriodicity m dirs t
        -- scanl (flip $ step m) "QBT" dirs
     in ( findZs $ scanl (flip $ step m) t $ concat $ replicate p dirs
        , p
        , findZs $ scanl (flip $ step m) (head cyc) $ concat $ replicate (length cyc) dirs
        , length cyc
        )
    where 
    zs = Set.filter ((=='Z') . T.last) $ Map.keysSet m
    findZs = Data.List.findIndices (`Set.member` zs)
    

step :: RouteMap -> Bool -> Text -> Text
step m dir t = (if dir then fst else snd) $ m Map.! t

type Input = ([Bool], [Line]) -- True = Left
parseInput :: P.Parser Input
parseInput = do
    directions <- P.takeWhile (not . isEndOfLine)
    endOfLine
    endOfLine

    ls <- sepBy1' parseLine endOfLine 
    skipSpace 
    endOfInput
    return (map (=='L') $ T.unpack directions, ls)

type Line = (Text, (Text, Text))

parseLine :: P.Parser Line
parseLine = do
    --- AAA = (QQV, JTL)
    a <- P.take 3
    " = ("
    b <- P.take 3
    ", "
    c <- P.take 3
    ")"
    return (a, (b, c))

-- * util
extendedEulidean 0 b = (b, 0, 1)
extendedEulidean a b = 
    let (q,r) = b `divMod` a
        (g, s, t) = extendedEulidean r a
     in (g, t - q * s, s)

crt2 (n,m) (n',m') =
    let (r, a, a') = extendedEulidean m m'
     in (n' * a * m + n * a' * m', m*m')
     
crt' :: [(Integer, Integer)] -> (Integer, Integer)
crt' = foldl' crt2 (0,1)

crt :: [(Integer, Integer)] -> Integer -- crt [(2,3),(3,5),(2,7)] == 23
crt = uncurry mod . crt'
