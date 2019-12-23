{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TransformListComp #-}
{-# language TupleSections #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Char
import Data.Foldable
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Array.Unboxed as UArray
import GHC.Exts (groupWith,the)

main = do
    s <- readInput <$> readFile "10.txt" 
    print $ p1 s
    print $ p2 s

type Coord = (Int,Int)

p1 :: Set Coord -> Int
p1 s = pred $ maximum $ map (length . viewers s) $ Set.toList s
p2 :: Set Coord -> Int
p2 s = 
    let c = monitoringStation s
        killlist = zipping $ killLines c (Set.toList s)
        (y,x) = killlist !! 199
     in x*100+y

monitoringStation :: Set Coord -> Coord
monitoringStation s = Data.List.maximumBy (compare `on` length . viewers s) $ Set.toList s

viewers s a = Set.filter (canSeeEachother s a) s
canSeeEachother s a b = not $ any (`Set.member` s) $ intermediate a b
     
intermediate :: Coord -> Coord -> [Coord]
intermediate (x1,y1) (x2,y2) =
    let dy = y1-y2
        dx = x1-x2
        p = gcd dx dy
        qy = dy `div` p
        qx = dx `div` p
     in if p <= 1 then [] else zip [x1-qx,x1-2*qx..x2+qx] [y1-qy,y1-2*qy..y2+qy]
     


killList :: Set Coord -> [Coord]
killList s = zipping $ killLines (monitoringStation s) (Set.toList s)

killLines :: Coord -> [Coord] -> [[Coord]]
killLines c = map (Data.List.sortOn (manhattenDist c)) 
            . toList 
            . Seq.sortOn (angleFromNorthTo c . head) 
            . foldl go mempty
            . filter (/=c)
    where
    go :: Seq [Coord] -> Coord -> Seq [Coord]
    go !acc c' = case Seq.findIndexL (colin c' . head) acc of
        Nothing -> [c'] Seq.:<| acc
        Just i -> Seq.adjust (c':) i acc
    
    colin a b = colinear (aToB c a) (aToB c b)

type Vec = (Int,Int)
    
(*.) :: Vec -> Vec -> Int
v1 *. v2 = fst v1 * fst v2 + snd v1 * snd v2

colinear :: Vec -> Vec -> Bool
colinear v1 v2 = let (a,b) = intcos v1 v2 in a>0 && a*a == b

intcos :: Vec -> Vec -> (Int,Int)
intcos v1 v2 = (v1 *. v2 , ((v1*.v1)*(v2*.v2)))

aToB :: Coord -> Coord -> Vec
aToB (x,y) (x',y') = (x'-x,y'-y)

{-
killLines' :: Coord -> [Coord] -> [[Coord]]
killLines' c cs = map snd -- $ Data.List.sort -- groupWith sorts
    [ (the ang,Data.List.sortOn (manhattenDist c) c')
    | c' <- cs
    , c' /= c
    , let ang = angleFromNorthTo c c'
    , then group by ang using groupWith
    ]
     
-- angleFromNorthTo :: Floating a => Coord -> Coord -> a
angleFromNorthTo :: Coord -> Coord -> Double
(y,x) `angleFromNorthTo` (y1,x1) =
    let dy = y1 - y
        dx = x1 - x
        d2 = dx*dx + dy*dy
        naiveAngle = acos $ fromIntegral (negate dy) / sqrt (fromIntegral d2)
     in case compare x x1 of
            LT -> naiveAngle      -- second coord on the right
            GT -> 2*pi-naiveAngle -- second coord on the left
            EQ -> if y >= y1 then 0 else pi
-}

zipping :: [[a]] -> [a]
zipping [] = []
zipping xs = 
    let ls = mapMaybe Data.List.uncons xs
     in map fst ls ++ zipping (map snd ls)

count :: Ord a => [a] -> [(a,Int)]
count xs = 
    [ (the x,length x)
    | x <- xs
    , then group by x using groupWith
    ]
     
readInput :: String -> Set Coord
readInput ls = Set.fromList
    [ (y,x)
    | (y,l) <- zip [0..] $ filter (not . null) $ lines ls
    , (x,c) <- zip [0..] l
    , c == '#'
    ]
    
manhattenDist :: Coord -> Coord -> Int
manhattenDist (x,y) (x',y') = abs (x-x') + abs (y-y')
    
blockChar = '█'

ex0 :: Set Coord -- 3,4 with 8
ex0=readInput".#..#\
\\n.....\
\\n#####\
\\n....#\
\\n...##"
ex1 :: Set Coord -- 6,3 with 41
ex1=readInput".#..#..###\
\\n####.###.#\
\\n....###.#.\
\\n..###.##.#\
\\n##.##.#.#.\
\\n....###..#\
\\n..#.#..#.#\
\\n#..#.#.###\
\\n.##...##.#\
\\n.....#.#.."

ex2:: Set Coord
ex2=readInput".#..##.###...#######\
\\n##.############..##.\
\\n.#.######.########.#\
\\n.###.#######.####.#.\
\\n#####.##.#.##.###.##\
\\n..#####..#.#########\
\\n####################\
\\n#.####....###.#.#.##\
\\n##.#################\
\\n#####.##.###..####..\
\\n..######..##.#######\
\\n####.##.####...##..#\
\\n.#####..#.######.###\
\\n##...#.##########...\
\\n#.##########.#######\
\\n.####.#.###.###.#.##\
\\n....##.##.###..#####\
\\n.#.#.###########.###\
\\n#.#.#.#####.####.###\
\\n###.##.####.##.#..##"

ex20 :: Set Coord
ex20 = readInput ".#....#####...#..\
\\n##...##.#####..##\
\\n##...#...#.#####.\
\\n..#.....#...###..\
\\n..#.#.....#....##"


drawKills :: Set Coord -> String
drawKills s = 
    let ms = monitoringStation s
        kills :: [Coord]
        kills = zipping $ killLines ms (Set.toList s)
        killSets :: [[Coord]]
        killSets = map (map snd) $ Data.List.groupBy ((==) `on` (`div` 9).fst) $ zip [0..] kills
        sets :: [Set Coord]
        sets = Data.List.scanl (\acc ls -> acc Set.\\ Set.fromList ls) s killSets
        ns = map ((,) <$> head <*> last) $ Data.List.groupBy ((==) `on` (`div` 9)) [0..]
     in Data.List.intercalate "\n\n" 
      $ zipWith (\(a,b) s -> unlines [('\t':show a) ++ " - " ++ ('\t':show b),s]) ns
      $ zipWith highlight (map (ms:) killSets) sets

highlight :: [Coord] -> Set Coord -> String
highlight cs s = unlines 
    [   [ if c `Set.notMember` s then '.' else case Data.List.findIndex (==c) cs of
            Nothing -> '#'
            Just 0  -> 'X'
            Just i  -> head $ show i
        | y<-[minimum $ Set.map snd s..maximum $ Set.map snd s]
        , let c = (x,y)
        ]
    | x<-[minimum $ Set.map fst s..maximum $ Set.map fst s]
    ]

ppr s = unlines 
    [   [if (y,x) `Set.notMember` s then '.' else head $ show $ length $ viewers s (y,x)
        |x<-[minimum $ Set.map fst s..maximum $ Set.map fst s]
        ]
    |y<-[minimum $ Set.map snd s..maximum $ Set.map snd s]
    ]
{-
.7..7
.....
67775
....7
...87
-}