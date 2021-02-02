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
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as UV
import qualified Data.List
import Data.Array
import GHC.Exts (the, groupWith)
import Text.Read

import qualified Data.Text as T

main :: IO ()
main = do
    s <- readFile "13x.txt"
    print $ part1 $ readInput s
    print $ part2 $ readInput' $ head . drop 1 . lines $ s
    s <- readFile "13.txt"
    print $ part1 $ readInput s
    print $ part2 $ readInput' $ head . drop 1 . lines $ s

part1 :: (Int,[Int]) -> Int
part1 (arrive, ms) = (\m -> m * wait m) $ minimumBy (compare `on` wait) ms
    where wait m = (m)-(arrive `mod` m)

readInput = go . lines
    where go (a:b:_) = (read a, ns)
            where ns = map (read . T.unpack) $ filter (/="x") $ T.splitOn "," $ T.pack b

part2 :: [Maybe Integer] -> Integer
part2 = crt . catMaybes . zipWith (fmap . (,)) [0,-1..]

readInput' = map (readMaybe . T.unpack) . T.splitOn "," . T.pack


-- crt :: [(Int,Int)] -> Int
-- crt = go . Data.List.sortOn snd
--     where
--     go [] = 0
--     go ((n,m):ts) = filterDown ts [n,n+m..]
--     filterDown [] ts = head ts
--     filterDown ((n,m):rs) ts = filterDown rs $ filter (\p -> (p `mod` m) == (n `mod` m)) ts

extendedEulidean 0 b = (b, 0, 1)
extendedEulidean a b = 
    let (q,r) = b `divMod` a
        (g, s, t) = extendedEulidean r a
     in (g, t - q * s, s)

crt2 (n,m) (n',m') =
    let (r, a, a') = extendedEulidean m m'
     in (n' * a * m + n * a' * m', m*m')
     
crt :: [(Integer,Integer)] -> Integer -- crt [(2,3),(3,5),(2,7)] == 23
crt = uncurry mod . foldl' crt2 (0,1)
