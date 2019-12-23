{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.Word
import qualified Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Array.Unboxed
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "14"
    let board = buildBoard s
    print $ length $ filter id $ elems board
    print $ findRegions board
    
s = "ugkiagan"


findRegions :: Board -> Int
findRegions board = 
    let closed = Set.filter (not . (board!)) allixs
     in go 0 (Set.findMin $ allixs Set.\\ closed) closed
    where
    bnds = bounds board
    allixs = Set.fromList $ range bnds
    go !i ix visited = 
        let cons = connectedTo board ix
            vis' = visited <> cons
         in maybe (i+1) (\ix' -> go (i+1) ix' vis') $ Set.lookupMin $ allixs Set.\\ vis'
        

connectedTo :: Board -> (Int,Int) -> Set (Int,Int)
connectedTo board ix = if not $ board ! ix then mempty else go (Set.singleton ix) (Set.singleton ix)
    where
    go new acc | null new = acc
    go new acc = 
        let new' = Set.filter (board !) $ foldMap (Set.fromList . neighbors board) new Set.\\ acc
         in go new' (new' <> acc)
    
    
neighbors :: Board -> (Int,Int) -> [(Int,Int)]
neighbors d (x,y) = filter (inRange $ bounds d)
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]

buildBoard :: String -> Board
buildBoard s = listArray bnds 
    [ testBit (hashes !! y !! word) (7-bit)
    | (x,y) <- range bnds
    , let (word,bit) = x `divMod` 8
    ]
    where
    bnds = ((0,0),(127,127))
    hashes = map knotHash $ map (\i -> s <> "-" <> show i) [0..127]


type Board = UArray (Int,Int) Bool
drawBoard :: Board -> String
drawBoard board = unlines
    [[tc $ board ! (x,y) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]
    ]
    where
    ((xmin,ymin),(xmax,ymax)) = bounds board
    tc b = if b then '#' else '.'

    
part1M :: [Int] -> State (Int,Int,Vector Int) ()
part1M = mapM_ (modify' . go)
    where
    go :: Int -> (Int,Int,Vector Int) -> (Int,Int,Vector Int)
    go l (p,s,v) = ((p + l + s) `mod` length v,s+1,move p l v)

knotHash :: String -> [Int]
knotHash s = -- mconcat $ map showHex' $ 
    let (a,b,c) = execState (replicateM 64 $ part1M ls) (0,0,Vector.fromList [0..255]) in go c
    where
    ls = map ord s ++ [17, 31, 73, 47, 23]
    go v = foldl1 xor <$> splitEvery 16 v
    
    
splitEvery :: Int -> Vector a -> [Vector a]
splitEvery n v 
    | null v = [] 
    | otherwise = let (a,b) = Vector.splitAt n v in a : splitEvery n b

showHex' :: Int -> String
showHex' n 
    | n < 16 = '0' : showHex n ""
    | otherwise = showHex n ""
    
-- move 0 3 (Vector.fromList [0..4]) == Vector.fromList [2,1,0,3,4]
-- move 3 4 (Vector.fromList [2,1,0,3,4]) == Vector.fromList [4,3,0,1,2]
-- move 1 5 (Vector.fromList [4,3,0,1,2]) == Vector.fromList [3,4,2,1,0]
move :: Int -> Int -> Vector Int -> Vector Int 
move _ l v | l <= 1 = v
move p l v | p + l <= length v = Vector.force $ a <> Vector.reverse b <> c
    where
    (a,v') = Vector.splitAt p v
    (b,c) = Vector.splitAt l v'
move p l v = Vector.force $ a' <> b <> c'
    where
    (v',c) = Vector.splitAt p v
    (a,b) = Vector.splitAt (l - Vector.length c) v'
    v'' = Vector.reverse $ c <> a
    (c',a') = Vector.splitAt (length c) v''