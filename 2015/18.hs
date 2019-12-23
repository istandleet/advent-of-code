{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Array.Unboxed

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "18"
    board <- getInput
    print $ onCount $ iterate animate board !! 100
    print $ onCount $ iterate animate' board !! 100
    
type Board = UArray (Int,Int) Bool

getInput :: IO Board
getInput = parseBoard <$> readFile "18.txt"

parseBoard :: String -> Board
parseBoard s = 
    array bnds 
        [ ((x,y),c=='#')
        | (y,l) <- zip [1..] $ lines s
        , (x,c) <- zip [1..] l
        ]
    where
    bnds = ((1,1),(length $ head $ lines s,length $ lines s))
    
animate :: Board -> Board
animate b = 
    array bnds 
        [ (i,go i)
        | i <- range bnds
        ]
    where
    bnds = bounds b
    go i = if b ! i
        then length (filter id $ neighbors i b) `elem` [2,3]
        else length (filter id $ neighbors i b) == 3
        
animate' :: Board -> Board
animate' b = 
    array bnds 
        [ (i,i `elem` corners || go i)
        | i <- range bnds
        ]
    where
    bnds = bounds b
    ((x,y),(x',y')) = bnds
    corners = [(x,y),(x',y'),(x,y'),(x',y)]
    go i = if b ! i
        then length (filter id $ neighbors i b) `elem` [2,3]
        else length (filter id $ neighbors i b) == 3
        
onCount :: Board -> Int
onCount = length . filter id . elems 
    
neighbors :: (Int,Int) -> Board -> [Bool]
neighbors (x,y) b = map (b !) is
    where
    is = [ i
         | dx <- [-1,0,1]
         , dy <- [-1,0,1]
         , let i = (x+dx,y+dy)
         , i /= (x,y)
         , bounds b `inRange` i
         ]