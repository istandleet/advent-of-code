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

import Data.Time

main :: IO ()
main = do
    input <- getInput
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print
    print $ part2 input
    getCurrentTime >>= print

part1, part2 :: Input -> Int
part1 = length . beacons
part2 = furthest . scanners
    where 
    furthest xs = maximum 
        [manhattanDist x y | (x:ys) <- Data.List.tails xs, y <- ys]

type Input = [(Int, [Coord])]
type Coord = (Int,Int,Int)

to3 :: Coord -> Coord -> Coord
(a,b,c) `to3` (x,y,z) = (x-a,y-b,z-c)

rotations = foldMap go
    [ \(a,b,c) -> (a,b,c)
    , \(a,b,c) -> (a,c,b)
    , \(a,b,c) -> (b,a,c)
    , \(a,b,c) -> (b,c,a)
    , \(a,b,c) -> (c,a,b)
    , \(a,b,c) -> (c,b,a)
    ]
    where go f = [\(x,y,z) -> f (q x,w y,e z) | q <- [negate, id], w <- [negate, id], e <- [negate, id]]

overlaps as xs = 
    [ (a,x):matches
    | (a:bs) <- Data.List.tails as 
    , length bs > 10
    , x <- xs
    , let matches = [(b,y) | b <- bs, y <- xs, y/=x, to3 a b `elem` map ($ to3 x y) rotations]
    , length matches > 10
    ]

integrate as xs = do 
    overlap <- listToMaybe $ overlaps as xs
    let ((a,x):(b,y):_) = overlap
        da = to3 a b 
        dx = to3 x y
    rotation <- find (\f -> da == f dx) rotations -- hope not symmetrical 
    let (q,w,e) = to3 (rotation x) a
    return $ (\(x,y,z) -> (x+q,y+w,z+e)) . rotation

beacons :: Input -> Set Coord
beacons (a:as) = go (Set.fromList $ snd a) $ map snd as 
    where
    go s [] = s
    go s (a:as) = case integrate (Set.toList s) a of 
        Nothing -> go s (as ++ [a])
        Just f  -> go (s <> Set.fromList (map f a)) as    
        -- let p xs = (\f -> map f xs) <$> integrate (Set.toList s) xs
        --     (t,f) = partitionMapMaybe p as 
        --  in if null t then error $ unwords ["failed with", show $ length as, "remaining"] else go (s <> foldMap Set.fromList t) f        

scanners :: Input -> [Coord]
scanners (a:as) = (0,0,0) : (go (Set.fromList $ snd a) $ map snd as )
    where
    go s [] = []
    go s (a:as) = case integrate (Set.toList s) a of 
        Nothing -> go s (as ++ [a])
        Just f  -> f (0,0,0) : go (s <> Set.fromList (map f a)) as    
        -- let p xs = (\f -> (f (0,0,0), map f xs)) <$> integrate (Set.toList s) xs
        --     (t,f) = partitionMapMaybe p as 
        --  in if null t then error $ unwords ["failed with", show $ length as, "remaining"] 
        --         else map fst t <> go (s <> foldMap (Set.fromList . snd) t) f        


{-# INLINE partitionMapMaybe #-}
partitionMapMaybe p xs = foldr (select p) ([],[]) xs
    
select p x ~(ts,fs) = case p x of 
    Nothing -> (ts, x:fs)
    Just y -> (y:ts, fs)


manhattanDist :: Coord -> Coord -> Int
manhattanDist (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')

getInput :: IO Input
getInput = readFile "19.txt" >>= either fail return . getInput'
getInput' :: String -> Either String Input
getInput' = P.parseOnly (P.sepBy1' parseScanner P.skipSpace <* P.skipSpace <* P.endOfInput) . T.pack

parseScanner = do
    "--- scanner "
    i <- P.decimal
    " ---\n"
    cs <- P.sepBy1' parseCoord "\n"
    return (i,cs)

parseCoord = do 
    x <- P.signed P.decimal
    char ','
    y <- P.signed P.decimal
    char ','
    z <- P.signed P.decimal
    return (x,y,z)
