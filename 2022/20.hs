{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
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
import Data.Attoparsec.Text as P hiding (takeWhile, take)

main :: IO ()
main = do
    input <- readFile "20.txt"
    let cs = map read $ filter (not . null) $ lines input :: [Integer]
    print $ part1 cs 
    print $ part2 $ map (*811589153) cs -- not -2617375018425

part1 :: [Integer] -> Integer
part1 cs = fin $ foldl (step l) init cs
    where 
    l = pred $ length cs
    init = Seq.fromList cs

part2 :: [Integer] -> Integer
part2 cs = fin $ iterate f init !! 10
    where 
    l = pred $ length cs
    init = Seq.fromList cs
    f circ = foldl (step l) circ cs

fin circ = 
    let (a, b) = Seq.breakl (==0) circ
        circ' = b <> a :|> 0
        l = length circ
     in Seq.index circ' (1000 `mod` l) + Seq.index circ' (2000 `mod` l) + Seq.index circ' (3000 `mod` l)

type Circle = Seq Integer
step :: Int -> Circle -> Integer -> Circle
step l circ 0 = circ 
step l circ n = 
    let (a, _ :<| b) = Seq.breakl (==n) circ
        (c, d) = Seq.splitAt (fromInteger (n `mod` fromIntegral l)) (b <> a)
     in c <> (n :<| d)

-- * Util
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)
