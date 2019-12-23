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
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text hiding (D)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "1"
    input <- getInput
    print $ part1 input
    print $ dist $ head $ part2 input

type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq,Ord,Show)


part1 :: [Line] -> Int
part1 ls = dist . snd $ execState (mapM_ (modify . move) ls) (U,(0,0))

part2 = go . map snd . scanl (flip move) (U,(0,0))
    where
    go coords = findDuplicates $ foldMap (tail . uncurry interpolateCoords) $ zip coords (tail coords)

dist (x,y) = abs x + abs y
    
findDuplicates :: Ord a => [a] -> [a]
findDuplicates = findDuplicatesBy id

interpolateCoords (x,y) (x',y')
    | y < y' = map (x,) [y..y']
    | y > y' = map (x,) $ reverse [y'..y]
    | x < x' = map (,y) [x..x']
    | x > x' = map (,y) $ reverse [x'..x]
    

findDuplicatesBy :: Ord a => (b -> a) -> [b] -> [b]
findDuplicatesBy f = snd . foldr folder (mempty,[]) 
    where folder x (s,acc) = let y = f x in if y `Set.member` s then (s,x:acc) else (Set.insert y s,acc)
    
move :: Line -> (Dir,Coord) -> (Dir,Coord)
move Line{..} (dir,coord) = (dir',moveBy dir' travel coord)
    where dir' = turn isright dir
    
turn :: Bool -> Dir -> Dir
turn isright d = case d of
    U -> if isright then R else L
    L -> if isright then U else D
    R -> if isright then D else U
    D -> if isright then L else R
    
moveBy :: Dir -> Int -> Coord -> Coord
moveBy dir d = case dir of 
    U -> _2 %~ (+d)
    L -> _1 %~ (subtract d)
    R -> _1 %~ (+d)
    D -> _2 %~ (subtract d)
    
    
getInput :: IO [Line]
getInput = readFile "1.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
data Line = Line
   { isright :: !Bool
   , travel :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine ", "

parseLine :: Parser Line
parseLine = do
    isright <- (True <$ char 'R') <|> (False <$ char 'L')
    travel <- decimal
    return Line{..}
    
    
    