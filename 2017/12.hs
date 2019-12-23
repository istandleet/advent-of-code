{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import Data.Attoparsec.Text


main = do
    putStrLn "12"
    ls <- getInput
    print $ part1 ls
    print $ part2 ls
    
readInput :: String -> [Line]
readInput = either error id . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
    
getInput :: IO [Line]
getInput = readInput <$> readFile "12.txt"
      
parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"


type Line = (Int,[Int])
parseLine :: Parser Line
parseLine = do
    i <- decimal
    " <-> "
    is <- sepBy1' decimal ", "
    return (i,is)
    
part1 :: [Line] -> Int
part1 ls = go start start
    where
    conns = IntMap.fromList ls
    start = IntSet.singleton 0
    go new visited
        | IntSet.null new = IntSet.size visited
        | otherwise = 
            let new' = foldMap IntSet.fromList (IntMap.restrictKeys conns new) IntSet.\\ visited
             in go new' (visited <> new)
             
part2 :: [Line] -> Int
part2 ls = go 1 start start
    where
    conns = IntMap.fromList ls
    start = IntSet.singleton 0
    allkeys = IntSet.fromList $ map fst ls
    go !i new visited
        | visited == allkeys = i
        | IntSet.null new = 
            let new' = IntSet.singleton $ IntSet.findMin $ allkeys IntSet.\\ visited
             in go (i+1) new' visited
        | otherwise = 
            let new' = foldMap IntSet.fromList (IntMap.restrictKeys conns new) IntSet.\\ visited
             in go i new' (visited <> new)
    
    
s = "0 <-> 2\n\
\1 <-> 1\n\
\2 <-> 0, 3, 4\n\
\3 <-> 2, 4\n\
\4 <-> 2, 3, 6\n\
\5 <-> 6\n\
\6 <-> 4, 5"