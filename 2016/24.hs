{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List
import Data.Array.Unboxed
import Data.Char (isDigit,digitToInt)

main = do
    putStrLn "24"
    input <- readInput <$> readFile "24.txt"
    print $ part1 input
    print $ part2 input
    
part1 :: Input -> Int
part1 input = minimum $ map (pathDistance ds) paths
    where
    ds = distances input
    cs = allCities ds
    paths = filter ((==0) . head)
          $ Data.List.permutations $ Set.toList cs
          
part2 :: Input -> Int
part2 input = minimum $ map (pathDistance ds) paths
    where
    ds = distances input
    cs = allCities ds
    paths = map ((0:) . reverse)
          $ filter ((==0) . head)
          $ Data.List.permutations $ Set.toList cs
    
distances :: Input -> [(Int,Int,Int)]
distances Input{..} = go positions
    where
    go [] = []
    go ((ix,i):ps) = zipWith (\j d -> (i,j,d)) (map snd ps) (calcDistances board ix $ map fst ps) ++ go ps
    
calcDistances :: Board -> (Int,Int) -> [(Int,Int)] -> [Int]
calcDistances board ix ixs = map (\ix' -> fromMaybe (-1) $ Data.List.findIndex (ix' `Set.member`) expansions) ixs
    where
    expansions = expanding board ix
    
expanding :: Board -> (Int,Int) -> [Set (Int,Int)]
expanding board ix = go mempty (Set.singleton ix)
    where
    go old new 
        | null new  = []
        | otherwise = new : go (old <> new) (expand board old new)
    
    
expand :: Board -> Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
expand board old new = Set.fromList
    [ ix
    | (x,y) <- Set.toList $ new
    , ix <- map (,y) [x-1,x+1] ++ map (x,) [y-1,y+1]
    , ix `Set.notMember` (old <> new)
    , inRange (bounds board) ix 
    , board ! ix
    ]
    

    
    
    
    
    
data Input = Input
    { positions :: [((Int,Int),Int)]
    , board     :: {-# unpack #-} !Board
    } deriving (Show, Eq)
type Board = UArray (Int,Int) Bool

readInput :: String -> Input    
readInput s = Input{..}
    where
    board     = listArray bnds $ map (\(x,y) -> ls !! y !! x /= '#') $ range bnds 
    positions = mapMaybe (\(x,y) -> ((x,y),digitToInt (ls !! y !! x)) <$ guard (isDigit $ ls !! y !! x)) $ range bnds
    ls = lines s
    bnds = ((0,0),(length (head ls)-1,length ls-1))

s = "###########\n\
\#0.1.....2#\n\
\#.#######.#\n\
\#4.......3#\n\
\###########"

     
travelingSalesman :: Ord a => [(a,a,Int)] -> Int
travelingSalesman ls =
    let cs = allCities ls
        paths = Data.List.permutations $ Set.toList cs
     in minimum $ map (pathDistance ls) paths

allCities :: Ord a => [(a,a,Int)] -> Set a
allCities ss = Set.fromList (map ffst ss) <> Set.fromList (map ssnd ss)
    where
    ffst (a,b,c)=a
    ssnd (a,b,c)=b
    
pathDistance :: Eq a => [(a,a,Int)] -> [a] -> Int
pathDistance ls cs = sum $ map go $ zip cs $ tail cs
  where go (a,b) = findDistance a b ls
  
findDistance :: Eq a => a -> a -> [(a,a,Int)] -> Int
findDistance a b ((a',b',i):ls)
    | (a == a' && b == b') || (b == a' && a == b') = i
    | otherwise = findDistance a b ls
    
    
    
    