{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "9"
    input <- getInput
    print $ travelingSalesman input
    print $ travelingSalesman' input

getInput :: IO [Line]
getInput = readFile "9.txt"
      >>= either fail return . parseOnly parseLines . T.strip . T.pack
      
type City = Text
type Line = (City,City,Int)

travelingSalesman :: [Line] -> Int
travelingSalesman ls =
    let cs = allCities ls
        paths = Data.List.permutations $ Set.toList cs
     in minimum $ map (pathDistance ls) paths
     
travelingSalesman' :: [Line] -> Int
travelingSalesman' ls =
    let cs = allCities ls
        paths = Data.List.permutations $ Set.toList cs
     in maximum $ map (pathDistance ls) paths

allCities :: [Line] -> Set City
allCities ss = Set.fromList (map ffst ss) <> Set.fromList (map ssnd ss)
    where
    ffst (a,b,c)=a
    ssnd (a,b,c)=b

findDistance :: City -> City -> [Line] -> Int
findDistance a b ((a',b',i):ls)
    | (a == a' && b == b') || (b == a' && a == b') = i
    | otherwise = findDistance a b ls

pathDistance :: [Line] -> [City] -> Int
pathDistance ls cs = sum $ map go $ zip cs $ tail cs
  where go (a,b) = findDistance a b ls

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" <* endOfInput
      
parseLine :: Parser Line
parseLine = do
    a <- word
    " to "
    b <- word
    " = "
    c <- decimal
    return (a,b,c)
    where
    word = T.pack <$> many1' letter <?> "wire"
    
s :: [Line]
s = [("a","b",464),("a","c",518),("c","b",141)]