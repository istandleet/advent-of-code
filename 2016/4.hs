{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import GHC.Exts (groupWith,the)

import Data.Char
import Data.Attoparsec.Text as P hiding (take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "4"
    input <- getInput
    print $ part1 input
    mapM_ print $ Data.List.sortOn snd 
                [(secid l,name l) | l <- input, genCheck l == checksum l]

part1 :: [Line] -> Int
part1 = sum . map secid . filter (\l -> genCheck l == checksum l)
    
genCheck :: Line -> String
genCheck = go . concat . parts
    where
    go cs = map fst $ take 5 $ Data.List.sortOn (\(c,n) -> (Down n, c))
        [ (the c, length c)
        | c<- cs
        , then group by c using groupWith
        ]

shift :: Int -> Char -> Char
shift s c = 
    let o = ord c 
        o' = ((ord c - 97 + s) `mod` 26) + 97
     in chr o'
     
name :: Line -> String
name Line{..} = unwords $ map (map $ shift secid) parts
        
getInput :: IO [Line]
getInput = readFile "4.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
data Line = Line
   { parts :: ![String]
   , secid :: {-# unpack #-} !Int
   , checksum :: !String
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = do
    parts <- sepBy1' word "-"
    char '-'
    secid <- decimal
    char '['
    checksum <- word
    char ']'
    return Line{..}
    where
    word = many1' letter
    
    