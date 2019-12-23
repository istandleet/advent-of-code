{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    putStrLn "13"
    print part1
    print $ let (a,b) = iterate step (mempty, Set.singleton (1,1)) !! 50 in length $ a <> b

part1 :: Int
part1 = find 31 39
    
find x y = go 0 mempty (Set.singleton (1,1))
    where
    go !i old new | i > 100 = error "didn't find it"
    go !i old new = if (x,y) `Set.member` new then i else go (i+1) (old <> new) (expand old new)
    
expanding :: (Set (Int,Int),Set (Int,Int)) -> IO (Set (Int,Int),Set (Int,Int))
expanding (a,b) = step (a,b) <$ putStrLn (draw' 10 10 (a <> b))
    
step :: (Set (Int,Int),Set (Int,Int)) -> (Set (Int,Int),Set (Int,Int))
step (old,new) = (old <> new, expand old new)
    
__favNumber = 1358

expand :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
expand old new = Set.fromList
    [ ix
    | (x,y) <- Set.toList $ new
    , ix <- map (,y) (filter (>=0) [x-1,x+1]) ++ map (x,) (filter (>=0) [y-1,y+1])
    , ix `Set.notMember` (old <> new)
    , uncurry isOpen ix
    ]

isOpen :: Int -> Int -> Bool
isOpen x y = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + __favNumber

draw :: Int -> Int -> String
draw x_ y_ = unlines 
    [[if isOpen x y then '.' else '#' | x <- [0..x_]] | y <- [0..y_]]
    
draw' :: Int -> Int -> Set (Int,Int) -> String
draw' x_ y_ visited = unlines 
    [[if (x,y) `elem` visited then 'O' else if isOpen x y then '.' else '#' | x <- [0..x_]] | y <- [0..y_]]