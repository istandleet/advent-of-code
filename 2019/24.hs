{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    print $ p1 $ readBoard input
    
    
p1 = biodiversityRating . firstDuplicate

firstDuplicate :: Board -> Board
firstDuplicate = head . findDuplicates . iterate step
    
-- * Robot
type Coord = (Int,Int)
type Board = UArray Coord Bool

step :: Board -> Board
step arr = listArray bnds $ map go (range bnds)
    where
    bnds = bounds arr
    -- go ix = if arr ! ix 
    --             then near == 1 -- A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
    --             else near == 1 || near == 2 -- An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
    go ix = near == 1 || (near == 2 && not (arr ! ix))
        where near = length $ filter (arr !) $ neighbors arr ix  

biodiversityRating :: Board -> Int
biodiversityRating = sum . map (2^). map (subtract 6) . map (\(x,y) -> 5*y+x) . map fst . filter snd . assocs 
        
-- ** Draw
readBoard :: String -> Board
readBoard s = array ((1,1),(5,5))
    [ ((x,y),c=='#')
    | (y,l) <- zip [1..5] $ lines s
    , (x,c) <- zip [1..5] $ l
    ]

drawBoard :: Board  -> String
drawBoard m = init $ unlines 
    [   [ if m !(x,y) then '#' else '.'
        |x<-[1..5]
        , let c = (x,y)
        ]
    | y <- [1..5]
    ]

-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State

-- * Utils
findDuplicates :: Ord a => [a] -> [a]
findDuplicates = go mempty
    where
    go !acc [] = []
    go !acc (x:xs) = if x `Set.member` acc then x:go acc xs else go (Set.insert x acc) xs

{-# specialize neighbors :: Board -> Coord -> [Coord] #-}
neighbors :: IArray arr e => arr Coord e -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d)
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
ex = "....#\
\\n#..#.\
\\n#..##\
\\n..#..\
\\n#...."

input = "#..##\
\\n#.#..\
\\n#...#\
\\n##..#\
\\n#..##"