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

main :: IO ()
main = do
    input <- map readLine . filter (not . null) . lines <$> readFile "25.txt"
    putStrLn $ part1 input
    
part1 :: Input -> String
part1 = toSNAFU' . sum . map fromSNAFU

type Input = [SNAFU]
type SNAFU = [Integer]

fromSNAFU :: SNAFU -> Integer 
fromSNAFU is = sum $ zipWith (*) (reverse is) $ iterate (*5) 1

fromSNAFU' = fromSNAFU . readLine 
toSNAFU' = map showsChar . toSNAFU

toSNAFU :: Integer -> SNAFU
toSNAFU = adjust . reverse . go
    where
    go 0 = []
    go n = let (q,r) = n `quotRem` 5 
            in if r < 3 then r : go q else (r-5) : go (q+1)

    adjust [] = []
    adjust (n:ns) | n > 0 = n : ns
                  | otherwise = 1 : reverse (go $ (1-n)*5^length ns + fromSNAFU ns)

readLine :: String -> SNAFU
readLine = map readChar 

readChar = \case
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> -1
    '=' -> -2

showsChar = \case
    2 -> '2'
    1 -> '1'
    0 -> '0'
    -1 -> '-'
    -2 -> '='