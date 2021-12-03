-- cat 03.txt | runghc 03.hs


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
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

main :: IO ()
main = do
    s <- getinput
    print $ part1 s
    print $ part2 s


-- part1 :: [Line] -> Int
part1 :: [[Int]] -> Int
part1 = f . mostcommon
    where 
    f ls = (sum $ map (\(p,j) -> 2^(length ls - p-1)*j) ls)
         * (sum $ map (\(p,j) -> 2^(length ls - p-1)*(1-j)) ls)

mostcommon iss = 
    [ (the p, if 2*sum i > length iss then 1 else 0)
    | is <- iss
    , (p,i) <- zip [0..] is
    , then group by p using groupWith
    ]

fromdigits :: Num a => [a] -> a
fromdigits = foldr (\a b -> 2*b + a) 0

-- getinput :: String -> [[Int]]
getinput = readFile "03.txt" >>= pure.  map (readints) . filter (not . null) . lines
readints :: String -> [Int]
readints = map $ read . pure

part2 :: [[Int]] -> Int
part2 iss = fromdigitsl (mostcommon' iss) * fromdigitsl (leastcommon' iss) 
mostcommon' [] = []
mostcommon' iss = 
    let is = map head iss 
        mc = if 2*sum is >= length is then 1 else 0
        iss' = filter (not . null) $ map tail $ filter ((==mc) . head) iss 
     in mc : mostcommon' iss'

fromdigitsl = foldl (\acc i -> acc*2+i) 0 

leastcommon' [] = []
leastcommon' iss = 
    let is = map head iss 
        lc = if 2*sum is >= length is then 0 else 1
        iss' = map tail $ filter ((==lc) . head) iss
        -- iss'' = if null iss' then replicate (length $ head iss) 0 else leastcommon' iss'
     in lc : if length iss' == 1 then head iss' else leastcommon' iss'
