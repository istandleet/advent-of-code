{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import Data.Array
import GHC.Exts (groupWith,the)

import Data.Char
import Data.Attoparsec.Text as P hiding (take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "18"
    print $ part1 input 40
    print $ part1 input 400000
    
part1 :: String -> Int -> Int
part1 s rows = length $ filter id $ elems $ trapFloor s rows
    
type Floor = Array (Int,Int) Bool
trapFloor :: String -> Int -> Floor
trapFloor s rows = safearr
    where
    bnds = ((1,0),(rows,length s -1))
    safearr = listArray bnds $ map (uncurry isSafe) $ range bnds
    
    isSafe 1 i = not $ s !! i == '^'
    isSafe r i = classify (get (r-1) (i-1)) (get (r-1) i) (get (r-1) (i+1))
    
    get r i = if inRange bnds (r,i) then safearr ! (r,i) else True
    
input = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
examp = ".^^.^.^^^^"

showFloor :: Floor -> String
showFloor safearr = unlines
    [ [if safearr ! (r,x) then '.' else '^' | x <- range (x_min,x_max)]
    | r <- range (y_min,y_max)
    ]
    where
    ((y_min,x_min),(y_max,x_max)) = bounds safearr

{-
Then, a new tile is a trap only in one of the following situations:
Its left and center tiles are traps, but its right tile is not.
Its center and right tiles are traps, but its left tile is not.
Only its left tile is a trap.
Only its right tile is a trap.
-}
classify :: Bool -> Bool -> Bool -> Bool
classify l c r = not $ 
    (not (l||c) && r)
 || (not (c||r) && l)
 || (not l && r && c)
 || (not r && l && c)