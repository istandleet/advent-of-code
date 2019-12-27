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
import qualified Data.Vector.Unboxed as V
import qualified Data.List
import Data.Array.Unboxed

import Intcode

main :: IO ()
main = do
    scaffprog <- read . (\s -> "["++s++"]") <$> readFile "17.txt" :: IO Program
    putStrLn $ getScaff scaffprog
    print $ p1 scaffprog -- 11140
    print $ p2hardcoded scaffprog -- 1113108
    
-- * P1
p1 :: Program -> Int
p1 = sum . map (uncurry (*)) . Set.toList . alignmentPoints . snd . parseScaffolding . getScaff

getScaff :: Program -> String
getScaff = map toEnum . getAllOuts . initComputer

getAllOuts :: Computer -> [Int]
getAllOuts = fix $ \rec c -> case runToPause c of 
    HasOutput (i,c) -> i : rec c
    _ -> []
    
alignmentPoints :: Board -> Set Coord
alignmentPoints brd = Set.filter isIntersection brd
    where isIntersection ix = length (neighbors ix `Set.intersection` brd) == 4
    
isScaffolding :: Char -> Bool
isScaffolding c = c `elem` "^v<>#"
    
-- * P2
type Board = Set Coord
data State = State
   { visited :: !(Set Coord)
   , pos :: !Coord
   , dir :: !Dir
   } deriving (Show,Eq)

parseScaffolding :: String -> (State,Board)
parseScaffolding = ((,) <$> initSt <*> Set.fromList . map fst) . filter (isScaffolding . snd) . assocs . charArray
    where 
    initSt ixcs = case Data.List.find ((`elem` "^v<>").snd) ixcs of
        Just (ix,c) -> State mempty ix $ toEnum $ head $ Data.List.findIndices (==c) "^<>v"

p2 :: Program -> Int
p2 scaffprog = 
    let (st,brd) = parseScaffolding $ getScaff scaffprog
        moveprog = V.force $ V.cons 2 (V.tail scaffprog)
     in undefined
     
p2hardcoded :: Program -> Int
p2hardcoded scaffprog = 
    let moveprog = V.force $ V.cons 2 (V.tail scaffprog)
     in last $ getAllOuts $ feedInput hardcodedInstructions $ initComputer moveprog

hardcodedInstructions = map fromEnum $ unlines
        [ "A,A,B,B,C,B,C,B,C,A"
        , "L,10,L,10,R,6"
        , "R,12,L,12,L,12"
        , "L,6,L,10,R,12,R,12"
        , "n" ]

feedInput :: [Int] -> Computer -> Computer
feedInput [] c = c
feedInput (i:is) c = case runToPause c of
    NeedsInput f -> feedInput is (f i)
    HasOutput (o,c') -> feedInput (i:is) c'
    {- HasOutput (o,c') -> error $ unlines
        [ "Unexpected Output!" 
        , map toEnum (o:getAllOuts c')
        , "Remaining Input:"
        , show (i:is)
        , map toEnum (i:is)
        ]
    -}


-- * Utils
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)

charArray :: String -> UArray Coord Char
charArray s = array ((0,0),(x1,y1))
    [ ((x,y),c)
    | (y,l) <- zip [0..y1] $ lines s
    , (x,c) <- zip [0..x1] l
    ]
    where
    y1 = pred $ length $ filter (not . null) $ lines s
    x1 = pred $ length $ head $ lines s
    
neighbors :: Coord -> Set Coord
neighbors (x,y) = Set.fromList
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
-- * Ex
ex = "..#..........\
\\n..#..........\
\\n#######...###\
\\n#.#...#...#.#\
\\n#############\
\\n..#...#...#..\
\\n..#####...^.."

ex2="#######...#####\
\\n#.....#...#...#\
\\n#.....#...#...#\
\\n......#...#...#\
\\n......#...###.#\
\\n......#.....#.#\
\\n^########...#.#\
\\n......#.#...#.#\
\\n......#########\
\\n........#...#..\
\\n....#########..\
\\n....#...#......\
\\n....#...#......\
\\n....#...#......\
\\n....#####......"
