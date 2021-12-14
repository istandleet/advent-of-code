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
    input <- getInput <$> readFile "12.txt"
    print $ part1 input
    print $ part2 input -- not 217184

-- part1 :: [Line] -> Int
part1 = length . go [(mempty, "start")] . buildBoard
    where
    go [] b = []
    go acc b = 
        let acc' = foldMap (step b) acc 
            (ended,going) = Data.List.partition ((=="end") . snd) acc'
         in ended ++ go going b

type St = (Set Text, Text)

step b (ts, t) =
    let next = filter (`Set.notMember` ts) $ b Map.! t
     in map (if T.toLower t == t then Set.insert t ts else ts,) next

part2 = length . go [(False, (mempty, "start"))] . buildBoard
    where
    go [] b = []
    go acc b = 
        let acc' = foldMap (st b) acc 
            (ended,going) = Data.List.partition ((=="end") . snd . snd) acc'
         in ended ++ go going b

    st b (True, s) = map (True,) $ step b s
    st b (False, s) = step2 b s

step2 b (ts, t) =
    let nxt = filter (/="start") $ b Map.! t
        (easy, hard) = Data.List.partition (`Set.notMember` ts) nxt
     in map (False,) (map (if T.toLower t == t then Set.insert t ts else ts,) easy)
     ++ map (True,) (map (if T.toLower t == t then Set.insert t ts else ts,) hard)

type Board = Map Text [Text]
buildBoard ss = Map.unionWith (<>) a d
    where 
    a = Map.fromList
        [ (the s, b) 
        | (s,b) <- ss
        , then group by s using groupWith
        ]
    d = Map.fromList
        [ (the b, s) 
        | (s,b) <- ss
        , then group by b using groupWith
        ]

type Line = (Text, Text)
getInput :: String -> [Line]
getInput = map (\(a,b) -> (T.pack a, T.pack b)) . map go . filter (not . null) . lines 
    where go = fmap tail . break (=='-')
