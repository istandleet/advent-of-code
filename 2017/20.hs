{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import GHC.Exts (the, groupWith)

import Data.Char
import Data.Attoparsec.Text hiding (D, count)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "20"
    
part1 :: [Line] -> Int
part1 is =
    let macc = minimum $ map (distance . acc) is
     in fst . head . filter ((==macc) . distance . acc . snd) $ zip [0..] is
    
    
distance :: Triplet -> Int
distance (x,y,z) = abs x + abs y + abs z
    
getInput :: IO [Line]
getInput = readFile "20.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
type Triplet = (Int,Int,Int)
      
data Line = Line
   { pos :: {-# unpack #-} !Triplet
   , vel :: {-# unpack #-} !Triplet
   , acc :: {-# unpack #-} !Triplet
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = do
    "p="
    pos <- parseTriplet
    ", v="
    vel <- parseTriplet
    ", a="
    acc <- parseTriplet
    return Line{..}
    
parseTriplet :: Parser Triplet
parseTriplet = do
    char '<'
    a <- signed decimal
    char ','
    b <- signed decimal
    char ','
    c <- signed decimal
    char '>'
    return (a,b,c)
    
count :: Ord a => [a] -> [(a,Int)]
count xs = [(the x, length x) | x <- xs, then group by x using groupWith]