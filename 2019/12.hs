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
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Array.Unboxed
import qualified Data.List

main :: IO ()
main = do
    putStrLn "Hello World"
    print $ p1 input
    print $ p2 input
    
p1 :: State -> Int
p1 input = energy $ iterate step input !! 1000

p2 :: State -> Int
p2 = longestNoRepeat' . iterate step

survey n input = Data.List.maximumBy (compare `on` energy) $ take n $ iterate step input 



longestNoRepeat :: Ord a => [a] -> [a]
longestNoRepeat = go mempty
    where
    go !acc [] = []
    go !acc (x:xs) = if x `Set.member` acc then [] else x:go (Set.insert x acc) xs
longestNoRepeat' :: Ord a=> [a] -> Int
longestNoRepeat' = go mempty 0
    where
    go !acc !n [] = n
    go !acc !n (x:xs) = if x `Set.member` acc then n else go (Set.insert x acc) (succ n) xs

-- * Vector
data Moon = Moon
   { position :: !(Vector Int)
   , velocity :: !(Vector Int)
   } deriving (Show, Eq, Ord)
   
type State = Vector Moon
mkState :: [Vector Int] -> State
mkState = V.fromList . map go where go v = Moon v (V.replicate (V.length v) 0)


step :: State -> State
step = updatePositions . updateVelocities
energy :: State -> Int
energy = sum . fmap (\(Moon p v) -> sum (fmap abs p) * sum (fmap abs v))

-- | Applies velocities
updatePositions :: State -> State
updatePositions = V.map go
    where go (Moon p v) = Moon (V.zipWith (+) p v) v
updateVelocities :: State -> State
updateVelocities ms = V.accum update ms (pairwise [0..V.length ms-1])
    where 
    update :: Moon -> [Int] -> Moon
    update = foldr (go . (ms V.!))
    go :: Moon -> Moon -> Moon
    go (Moon p v) (Moon p' v') = Moon p' (V.zipWith3 gravity p p' v')
    gravity :: Int -> Int -> Int -> Int
    gravity p p' = case compare p p' of
        GT -> succ
        EQ -> id
        LT -> pred

-- * Array
data Axis = X | Y | Z deriving (Show, Eq, Ord, Enum, Bounded, Ix)

-- * Utils
pairwise :: [a] -> [(a,[a])]
pairwise [] = []
pairwise (x:xs) = (x,xs) : map (_2 %~ (x:)) (pairwise xs)

-- * Inputs
example :: State
example = mkState $ map V.fromList
    [[-1,0,2]
    ,[2,-10,-7]
    ,[4,-8,8]
    ,[3,5,-1]]
example2 :: State
example2 = mkState $ map V.fromList
    [[-8,-10,0]
    ,[5,5,10]
    ,[2,-7,3]
    ,[9,-8,-3]]
    
input :: State
input = mkState $ map V.fromList
    [[-17, 9 , -5]
    ,[-1 , 7 , 13]
    ,[-19, 12, 5 ]
    ,[-6 , -6, -4]]