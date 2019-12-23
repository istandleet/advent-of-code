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
import Data.Attoparsec.Text


main = do
    putStrLn "11"
    dirs <- getInput
    print $ length $ part1 dirs
    print $ part2 dirs
    
    
    
    
    
    
    
part1 :: [Dir] -> Seq Dir
part1 = foldr merge mempty
part2 :: [Dir] -> Int
part2 = maximum . map length . scanl (flip merge) mempty
    
    
readInput :: String -> [Dir]
readInput = either error id . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
    
getInput :: IO [Dir]
getInput = readInput <$> readFile "11.txt"
      
parseLines :: Parser [Dir]
parseLines = sepBy1' parseDir ","

parseDir :: Parser Dir
parseDir = NE <$ "ne" 
       <|> NW <$ "nw" 
       <|> N  <$ "n" 
       <|> SE <$ "se" 
       <|> SW <$ "sw" 
       <|> S  <$ "s"     
    
    
    
    
data Dir = N | NE | SE | S | SW | NW deriving (Eq,Show,Ord,Enum,Bounded)

flipDir :: Dir -> Dir 
flipDir N  = S 
flipDir S  = N 
flipDir NE = SW
flipDir SE = NW
flipDir NW = SE
flipDir SW = NE
       
       
cancel :: Dir -> Dir -> Maybe (Maybe Dir)
cancel d d' | d == flipDir d' = Just Nothing
cancel NW NE = Just (Just N )
cancel N  SE = Just (Just NE)
cancel NE S  = Just (Just SE)
cancel SE SW = Just (Just S )
cancel S  NW = Just (Just SW)
cancel SW N  = Just (Just NW)
cancel NE NW = Just (Just N )
cancel SE N  = Just (Just NE)
cancel S  NE = Just (Just SE)
cancel SW SE = Just (Just S )
cancel NW S  = Just (Just SW)
cancel N  SW = Just (Just NW)
cancel _ _ = Nothing


merge :: Dir -> Seq Dir -> Seq Dir
merge d s = case Seq.findIndexL (isJust . cancel d) s of 
    Nothing -> d Seq.:<| s
    Just i  -> case join $ cancel d (Seq.index s i) of 
        Nothing -> Seq.deleteAt i s
        Just d' -> merge d' $ Seq.deleteAt i s
        