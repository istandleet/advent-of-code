{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import Data.Array
import qualified Data.List
import qualified Data.Set as Set
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as CLBS

import Data.Char
import Data.Attoparsec.Text hiding (D)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "17"
    putStrLn $ part1 "hhhxzeay"
    print $ part2 "hhhxzeay"

type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq,Ord,Show)

part1 :: String -> String
part1 k = drop (length k) $ go [(k,(0,0))]
    where
    go ks = maybe (go $ step ks) fst $ Data.List.find (((3,3)==).snd) ks 

part2 :: String -> Int
part2 k = go 1 0 [(k,(0,0))]
    where
    go !i !m ks = if null ks' then m' else go (i+1) m' ks'
        where
        m' = if null ws then m else i
        (ws,ks') = Data.List.partition (((3,3)==).snd) $ step ks


step :: [(String,(Int,Int))] -> [(String,(Int,Int))]
step = foldMap $ uncurry $ \k coord -> 
    [ (k ++ show dir,c')
    | dir <- dirs k
    , let c' = moveBy dir 1 coord
    , inRange bnds c'
    ]
    where
    bnds = ((0,0),(3,3))
        
dirs :: String -> [Dir]
dirs k = map fst $ filter ((`Set.member` goodChars) . snd) $ zip [U,D,L,R] $ md5s k


goodChars = Set.fromList ['b'..'f']

moveBy :: Dir -> Int -> Coord -> Coord
moveBy dir d = case dir of 
    L -> _1 %~ (subtract d)
    R -> _1 %~ (+d)
    -- note this is reversed DU
    D -> _2 %~ (+d)
    U -> _2 %~ (subtract d)
    
md5s :: String -> String
md5s = show . md5 . CLBS.pack