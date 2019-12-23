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
import Data.Attoparsec.Text


main = do
    putStrLn "9"
    s <- readFile "9.txt"
    print $ part1 $ T.pack s
    print $ part2 $ T.pack s
    
    
    
-- parse acc ('<':s) = parse acc $ skipGarbage s
-- parse acc ('{':s) = 

part1 = parseOnly (mainParser 1)
part2 = parseOnly mainParser2

mainParser :: Int -> Parser Int
mainParser i = parseGroup i <|> (0 <$ skipGarbage)

parseGroup :: Int -> Parser Int
parseGroup i = do
    char '{'
    is <- mainParser (succ i) `sepBy'` (char ',')
    char '}'
    return (i+sum is)
    
mainParser2 :: Parser Int
mainParser2 = parseGroup2 <|> skipGarbage

parseGroup2 :: Parser Int
parseGroup2 = do
    char '{'
    is <- mainParser2 `sepBy'` (char ',')
    char '}'
    return (sum is)

skipGarbage :: Parser Int
skipGarbage = do
    char '<'
    go 0
    where 
    go !i = do
        c <- anyChar
        case c of 
            '>' -> return i
            '!' -> anyChar *> go i
            _ -> go (succ i)

testsg = all (isRight . parseOnly skipGarbage) 
  ["<>",
  "<random characters>",
  "<<<<>",
  "<{!>}>",
  "<!!>",
  "<!!!>>",
  "<{o\"i!a,<{i<a>"]