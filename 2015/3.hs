{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "3"
    s <- readFile "3.txt"
    print $ part1 s
    print $ part2 s
    
part1 :: String -> Int
part1 = length . fst . foldl go (Set.singleton (0,0),(0,0))
  where
  go (s,p) c = 
        let p' = move p c
         in (Set.insert p' s,p') 
  
  move (x,y) '^' = (x,y+1)
  move (x,y) 'v' = (x,y-1)
  move (x,y) '<' = (x-1,y)
  move (x,y) '>' = (x+1,y)
  
type Pos = (Int,Int)
  
part2 :: String -> Int
part2 = go (Set.singleton (0,0),(0,0),(0,0))
  where
  go :: (Set.Set Pos, Pos, Pos) -> String -> Int
  go (s,ps,pr) [] = length s
  go (s,ps,pr) (cs:cr:css) = 
        let ps' = move ps cs
            pr' = move pr cr
         in go (Set.insert pr' $ Set.insert ps' s,ps',pr') css
  
  move (x,y) '^' = (x,y+1)
  move (x,y) 'v' = (x,y-1)
  move (x,y) '<' = (x-1,y)
  move (x,y) '>' = (x+1,y)