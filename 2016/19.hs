{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UVector
import Data.Array
import qualified Data.Sequence as Seq
import Numeric (showIntAtBase,readInt)
import Data.Char (intToDigit)
import GHC.Exts (groupWith,the)

import Data.Char
import Data.Attoparsec.Text as P hiding (take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "19"
    
part1 :: Int -> Int
part1 n = succ $ shiftL (clearBit n (logBase2 n)) 1
    
part1' :: Int -> Int
part1' i = go $ UVector.generate i succ
    where
    go v = if UVector.length v == 1 then UVector.head v else go $ UVector.force $ step v
    
step :: Vector Int -> Vector Int
step vs = UVector.ifilter go vs
    where
    l = UVector.length vs
    go _ _ | l == 1 = True
    go i _ | even l = even i
    go i _ = even i && i > 0
    
part2 :: Int -> Int
part2 i = case toTrinary $ pred i of
    ('1':s) -> maybe 0 succ $ readBase 3 s
    _ -> 2*i - roofpow3
    where
    roofpow3 = head $ dropWhile (<i) $ map (3^) [1..]

part2' :: Int -> Int
part2' i = go 0 i $ Seq.fromList [1..i]
    where
    go !i !l v
        | l == 1 = v `Seq.index` 0
        | i >= l = go 0 l v
        | otherwise =
            let d = (i + l `div` 2) `mod` l
             in go (if d < i then i else succ i) (pred l) $ Seq.deleteAt d v
    
toBinary :: Int -> String
toBinary n = showIntAtBase 2 intToDigit n ""
toTrinary :: Int -> String
toTrinary n = showIntAtBase 3 intToDigit n ""

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

readBase :: Int -> String -> Maybe Int
readBase n = fmap fst . listToMaybe . readInt n (`elem` (map intToDigit [0..n-1])) digitToInt