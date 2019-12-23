{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List
import Data.Array.Unboxed 

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Primes


main = do
    putStrLn "3"
    print $ part1 361527
    
    
    
part1 :: Int -> Int
part1 n = let (x,y) = part1' n in abs x + abs y
    
part1' :: Int -> (Int,Int)
part1' n 
    | additionalSteps == 0 = (br,br)
    | n <= trn  = (br+1,br+1-additionalSteps)
    | n <= tln  = (br+1-(additionalSteps-(integerSquareRoot brn)-1),-br-1)
    | n <= bln  = (-br-1,-br-1+(additionalSteps-2*(integerSquareRoot brn)-2))
    | otherwise = (-br-1+(n-bln),br+1)
    where
    (brn,trn,tln,bln) = corners n
    br = integerSquareRoot brn `div` 2
    additionalSteps = n - brn
    
corners n = (brn,trn,tln,bln)
    where
    isr = integerSquareRoot n
    brc = if odd isr then isr else isr - 1
    brn = brc*brc
    trn = (brn+tln) `div`2
    tln = if even isr then isr*isr+1 else (isr+1)*(isr+1)+1
    bln = (brc+2)*(brc+2)-(brc+1)


expand :: UArray (Int,Int) Int -> UArray (Int,Int) Int
expand arr = arr'
    where
    bnds@(_,(_,d)) = bounds arr
    bnds' = ((-d',-d'),(d',d'))
    d' = succ d
    arr' = array bnds'
        [ (ix,go ix)
        | ix <- range  bnds'
        ]
    go ix
        | inRange bnds ix = arr ! ix
        | otherwise = sum [arr' ! ix' | ix' <- neighbors ix, asInt ix' < asInt ix]
        
neighbors (x,y) = 
    [ (x',y')
    | x' <- [x-1,x,x+1]
    , y' <- [y-1,y,y+1]
    , (x,y) /= (x',y')
    ]
    
    
asInt (x,y) | x >= 0 && x == y = (2*x+1)*(2*x+1)
asInt (x,y) = 
    let m = max (abs x) (abs y) 
        ltbl = (2*m-1)*(2*m-1)
     in ltbl + (m-x) + (m-y) -- BAD
    
    
        
initialp2 :: UArray (Int,Int) Int
initialp2 = array ((0,0),(0,0)) [((0,0),1)]