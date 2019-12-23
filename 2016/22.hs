{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens hiding (Index)
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Array.Unboxed
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "22"
    
getData :: IO Input
getData = read <$> readFile "22t.txt"
    
type Index = (Int,Int)
type Input =  [(Int,Int,Int,Int,Int)]
type Dat = Array Index (Int,Int,Int)
transfo :: Input -> Dat
transfo dat = array bnds $ map (\d ->((d^. _1,d^. _2),(d^._3,d^._4,d^. _5))) dat
    where
    bnds = ((minimum $ map (^. _1) dat,minimum $ map (^. _2) dat)
           ,(maximum $ map (^. _1) dat,maximum $ map (^. _2) dat))
    
{-
Node A is not empty (its Used is not zero).
Nodes A and B are not the same node.
The data on node A (its Used) would fit on node B (its Avail).
-}

pairs :: Dat -> [(Index,Index)]
pairs dat = 
    [ (ix2,ix1)
    | ix1 <- range $ bounds dat
    , ix2 <- range $ bounds dat
    , ix1 /= ix2
    , let (_,used,_ ) = dat ! ix2
    , let (_,_,avail) = dat ! ix1
    , used > 0
    , used <= avail 
    ]
    
neighbors :: Dat -> Index -> [Index]
neighbors d (x,y) = filter (inRange $ bounds d) 
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
deadSpots :: Dat -> [Index]
deadSpots dat = 
    [ ix
    | ix <- range $ bounds dat
    , let (_,used ,_ ) = dat ! ix
    , not $ or [total>=used | ix2 <- neighbors dat ix, let (total,_,_ ) = dat ! ix2]
    ]