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
    putStrLn "3"
    input <- getInput
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Int
part1 = length . filter isTriangle
    
isTriangle :: Line -> Bool
isTriangle Line{..} = 
    let [x,y,z] = Data.List.sort [a,b,c]
     in x + y > z
     
part2 = length . filter isTriangle . transpose
     
transpose :: [Line] -> [Line]
transpose = foldMap ter . group3

ter :: (Line,Line,Line) -> [Line]
ter (x,y,z) =
    [Line{a=a x
         ,b=a y
         ,c=a z}
    ,Line{a=b x
         ,b=b y
         ,c=b z}
    ,Line{a=c x
         ,b=c y
         ,c=c z}
    ]
group3 [] = []
group3 (a:b:c:cs) = (a,b,c):group3 cs
    
getInput :: IO [Line]
getInput = readFile "3.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
data Line = Line
   { a :: {-# unpack #-} !Int
   , b :: {-# unpack #-} !Int
   , c :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = do
    skipSpace
    a <- decimal
    skipSpace
    b <- decimal
    skipSpace
    c <- decimal
    return Line{..}
    
    
    