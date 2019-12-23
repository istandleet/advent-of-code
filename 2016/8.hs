{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import Data.Array.Unboxed
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text hiding (D,take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "8"
    input <- getInput
    print $ part1 input
    putStrLn $ drawBoard $ part2 input
    

type Board = UArray (Int,Int) Bool
drawBoard :: Board -> String
drawBoard board = unlines
    [[tc $ board ! (x,y) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]
    ]
    where
    ((xmin,ymin),(xmax,ymax)) = bounds board
    tc b = if b then '#' else '.'

part1 :: [Line] -> Int
part1 = length . filter id . elems . part2
  
part2 :: [Line] -> Board
part2 = foldl (flip move) initial
  where initial = listArray ((0,0),(49,5)) $ repeat False

move :: Line -> Board -> Board
move line board = board // updates
    where
    (_,(xmax,ymax)) = bounds board
    updates = case line of 
        Rect   a b -> [((x,y),True) | x <- [0..(a-1) `min` xmax], y <- [0..(b-1)`min`ymax]]
        RotRow y b -> [((x,y),board ! ((x-b)`mod`(xmax+1),y))|x<-[0..xmax]]
        RotCol x b -> [((x,y),board ! (x,(y-b)`mod`(ymax+1)))|y<-[0..ymax]]
  
    
getInput :: IO [Line]
getInput = readFile "8.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
data Line = 
     Rect {-# unpack #-} !Int {-# unpack #-} !Int
   | RotRow {-# unpack #-} !Int {-# unpack #-} !Int
   | RotCol {-# unpack #-} !Int {-# unpack #-} !Int
   deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = rect <|> rotrow <|> rotcol
  where
  rect = do
    "rect "
    a <- decimal
    char 'x'
    b <- decimal
    return $ Rect a b
  rotcol = do
    "rotate column x="
    a <- decimal
    " by "
    b <- decimal
    return $ RotCol a b
  rotrow = do
    "rotate row y="
    a <- decimal
    " by "
    b <- decimal
    return $ RotRow a b
    
s :: Text
s = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"

test1 :: [Line] -> Int
test1 = length . filter id . elems . foldl (flip move) initial
  where initial = listArray ((0,0),(6,2)) $ repeat False