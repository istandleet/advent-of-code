{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language TupleSections #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List

import Data.Attoparsec.Text hiding (D,take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    (a,b) <- getInput
    print $ p1 a b
    print $ p2 a b
    
p1 :: [Move] -> [Move] -> Int
p1 a b = let sa = visitedSquares a; sb = visitedSquares b
             comm = sa `Set.intersection` sb
             dists = Set.map (\(x,y) -> abs x + abs y) comm
          in Set.findMin dists
p2 :: [Move] -> [Move] -> Int
p2 a b = let sa = signalDelays a; sb = signalDelays b
             dists = Map.intersectionWith (+) sa sb
          in minimum $ Map.elems dists



visitedSquares :: [Move] -> Set Coord
visitedSquares = fst . Data.List.foldl' (uncurry go) (mempty,(0,0))
    where
    go :: Set Coord -> Coord -> Move -> (Set Coord, Coord)
    go !acc !coord (dir,d) =
        let visits = take d $ tail $ iterate (move dir) coord
         in (acc <> Set.fromList visits,last visits)
    
signalDelays :: [Move] -> Map Coord Int
signalDelays = fst . Data.List.foldl' go (mempty,((0,0),0))
    where
    go :: (Map Coord Int, (Coord, Int)) -> Move -> (Map Coord Int, (Coord, Int))
    go (!acc,pos) (dir,d) =
        let visits = take d $ tail $ iterate (over _1 (move dir) . over _2 succ) pos
         in (acc <> Map.fromList visits,last visits)
    
    
type Move = (Dir,Int)
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Ord)
move :: Dir -> Coord -> Coord
move dir = case dir of 
    U -> _2 %~ succ
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ pred
    
moveBy :: Dir -> Int -> Coord -> Coord
moveBy dir d = case dir of 
    U -> _2 %~ (+d)
    L -> _1 %~ (subtract d)
    R -> _1 %~ (+d)
    D -> _2 %~ (subtract d)
    
data Wire = HWire Int Wire | VWire Int Wire | Ending deriving (Show)
getInput' :: IO (Wire,Wire)
getInput' = go . lines <$> readFile "03.txt" where go (a:b:_) = (parseWire a, parseWire b)
parseWire :: String -> Wire
parseWire = readWire . fmap T.unpack . T.splitOn "," . T.pack
readWire :: [String] -> Wire
readWire = foldr parseEach Ending
    where parseEach ('R':n) lw = HWire   (read n)  lw
          parseEach ('L':n) lw = HWire (-(read n)) lw
          parseEach ('U':n) lw = VWire   (read n)  lw
          parseEach ('D':n) lw = VWire (-(read n)) lw
positions :: Wire -> [(Int, Int)]
positions = Data.List.nub . concat . go 0 0
    where -- generates 1..n or -1..n for negatives, so we can step at each point along the path
          nRange !n = if n < 0 then negate <$> [1..(abs n)] else [1..n]
          go !x !y Ending           = [[]]
          go !x !y (HWire dx nextW) = ((,y) . (+x) <$> nRange dx):(go (x + dx) y nextW)
          go !x !y (VWire dy nextW) = ((x,) . (+y) <$> nRange dy):(go x (y + dy) nextW)
positions' :: Wire -> Set (Int, Int)
positions' = Set.fromList . concat . go 0 0
    where -- generates 1..n or -1..n for negatives, so we can step at each point along the path
          nRange !n = if n < 0 then negate <$> [1..(abs n)] else [1..n]
          go !x !y Ending           = [[]]
          go !x !y (HWire dx nextW) = ((,y) . (+x) <$> nRange dx):(go (x + dx) y nextW)
          go !x !y (VWire dy nextW) = ((x,) . (+y) <$> nRange dy):(go x (y + dy) nextW)
    
    
getInput :: IO (Line,Line)
getInput = readFile "03.txt"
      >>= either fail return . parseOnly (parseInput <* skipSpace <* endOfInput) . T.pack
    
parseInput :: Parser (Line,Line)
parseInput = (,) <$> (parseLine <* skipSpace) <*> parseLine
    
type Line = [Move]
parseLine :: Parser Line
parseLine = sepBy1' parseMove (char ',')

parseMove :: Parser Move
parseMove = (,) <$> parseDir <*> decimal

parseDir :: Parser Dir
parseDir = U <$ char 'U'
       <|> L <$ char 'L'
       <|> R <$ char 'R'
       <|> D <$ char 'D'
    

    
ex = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"