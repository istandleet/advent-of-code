{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Main where 

import Data.Foldable
import Data.Function
import Data.Attoparsec.Text
import qualified Data.Text as T

main = do
    putStrLn "23"
    lines <- getInput
    print $ part1 lines
  
  
part1 :: [Line] -> Int
part1 lines = length $ filter inrange lines
  where
  strongest = maximumBy (compare `on` radius) lines 
  inrange l = distance strongest l <= radius strongest
  
distance :: Line -> Line -> Int
distance l1 l2 = 
    abs (x l1 - x l2) +
    abs (y l1 - y l2) +
    abs (z l1 - z l2)

  
getInput :: IO [Line]
getInput = readFile "23.txt"
      >>= either fail return . mapM (parseOnly parseLine) . T.lines . T.pack
  
data Line = Line 
   { x      :: {-# unpack #-} !Int
   , y      :: {-# unpack #-} !Int
   , z      :: {-# unpack #-} !Int
   , radius :: {-# unpack #-} !Int
   } deriving (Eq,Show)
parseLine :: Parser Line
parseLine = do
    -- pos=<-13936329,35619897,41211497>, r=68603272
    "pos=<"
    x <- signed decimal
    char ','
    y <- signed decimal
    char ','
    z <- signed decimal
    ">, r="
    radius <- decimal
    return Line{..}