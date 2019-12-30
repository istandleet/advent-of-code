{-# language BangPatterns #-}
module Main where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    print $ p1 input -- 8742
    print $ periodicity $ iterate stepAxis $ x input
    print $ periodicity $ iterate stepAxis $ y input
    print $ periodicity $ iterate stepAxis $ z input
    print $ p2 input

p1 :: State -> Int
p1 input = energy $ iterate step input !! 1000

p2 :: State -> Int
p2 = foldl1 lcm . checkRem . axisPeriodicity
    where 
    checkRem xs = if not $ all ((==0).snd) xs then error "no zero rem" else map fst xs

axisPeriodicity :: State -> [(Int,Int)]
axisPeriodicity (State x y z) = let f = periodicity . iterate stepAxis in [f x, f y, f z]

periodicity :: Ord a=> [a] -> (Int,Int)
periodicity = go mempty 0
    where
    go !acc !n (x:xs) = case Map.lookup x acc of 
        Just offset -> (n-offset,offset) 
        Nothing -> go (Map.insert x n acc) (succ n) xs

-- * Vector
type PV = (Int,Int)
data State = State
    { x :: Vector PV
    , y :: Vector PV
    , z :: Vector PV
    } deriving (Show,Eq,Ord)
    
mkState :: [[Int]] -> State
mkState ins = State (go 0) (go 1) (go 2)
    where go i = V.fromList $ map (\is -> (is !! i,0)) ins

step :: State -> State
step (State x y z) = State (stepAxis x) (stepAxis y) (stepAxis z)

energy :: State -> Int
energy (State x y z) = 
    let (xp,xv) = V.unzip x
        (yp,yv) = V.unzip y
        (zp,zv) = V.unzip z
        pe = V.zipWith go (V.zipWith go xp yp) zp
        ke = V.zipWith go (V.zipWith go xv yv) zv
     in V.sum $ V.zipWith (*) pe ke
    where go a b = abs a + abs b

stepAxis :: Vector PV -> Vector PV
stepAxis = updatePositions . updateVelocities

-- | Applies velocities
updatePositions :: Vector PV -> Vector PV
updatePositions = V.map (\(p,v) -> (p+v,v))
    
updateVelocities :: Vector PV -> Vector PV
updateVelocities v = V.accum update v (pairwise [0..V.length v-1])
    where 
    gravity :: Int -> Int -> Int -> Int
    gravity p p' = (+ signum (p-p'))
    ps :: Vector Int
    ps = fst $ V.unzip v
    update :: PV -> [Int] -> PV
    update = foldr (app . (ps V.!))
    app :: Int -> PV -> PV
    app p' (p,v) = (p,gravity p' p v)
        
-- * Utils
pairwise :: [a] -> [(a,[a])]
pairwise [] = []
pairwise (x:xs) = (x,xs) : map (_2 %~ (x:)) (pairwise xs)

-- * Inputs
example :: State
example = mkState 
    [[-1,0,2]
    ,[2,-10,-7]
    ,[4,-8,8]
    ,[3,5,-1]]

example2 :: State
example2 = mkState 
    [[-8,-10,0]
    ,[5,5,10]
    ,[2,-7,3]
    ,[9,-8,-3]]
    
input :: State
input = mkState 
    [[-17, 9 , -5]
    ,[-1 , 7 , 13]
    ,[-19, 12, 5 ]
    ,[-6 , -6, -4]]