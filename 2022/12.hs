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

main :: IO ()
main = do
    input <- readFile "12.txt" :: IO String
    let (s, e, board) = readBoard input
    print $ part1 s e board
    print $ part2 e board

part1 :: Coord -> Coord -> Board -> Int
part1 s e b = distances b s Map.! e

part2 :: Coord -> Board -> Int
part2 e b = minimum 
    [ d
    | c <- range $ bounds b
    , b ! c == 0
    , d <- maybeToList $ Map.lookup e $ distances b c
    ]

distances :: Board -> Coord -> Map Coord Int
distances b s = go mempty (Set.singleton s) (Map.singleton s 0)
    where
    -- go :: Set Coord -> Set Coord -> Map Coord Int -> Map Coord Int
    go !visited !tovisit !d | null tovisit = d
    go !visited !tovisit !d = 
        let c = minimumBy (compare `on` (d Map.!)) tovisit
            visited' = Set.insert c visited
            ns = [(ix, d Map.! c + 1) | ix <- neighbors b c, not $ Set.member ix visited', b ! ix <= (b ! c) + 1]
            tovisit' = Set.union tovisit (Set.fromList $ map fst ns) `Set.difference` visited'
            d' = Map.unionWith min d $ Map.fromList ns
         in go visited' tovisit' d'

neighbors :: Board -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d) 
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]


type Coord = (Int, Int)
type Board = Array Coord Int

readBoard :: String -> (Coord, Coord, Board)
readBoard input = (s, e, array ((0,0), mxbnds) arr)
    where
    arr = [ ((x,y), readChar c)
          | (y, l) <- zip [0..] $ lines input
          , not $ null l
          , (x, c) <- zip [0..] l
          ]
    s = head 
        [ (x,y)
        | (y, l) <- zip [0..] $ lines input
        , not $ null l
        , (x, c) <- zip [0..] l
        , c == 'S'
        ]
    e = head 
        [ (x,y)
        | (y, l) <- zip [0..] $ lines input
        , not $ null l
        , (x, c) <- zip [0..] l
        , c == 'E'
        ]
    mxbnds = maximum $ map fst arr
    readChar 'S' = readChar 'a'
    readChar 'E' = readChar 'z'
    readChar c = fromEnum c - fromEnum 'a'
