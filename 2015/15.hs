{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts(the,groupWith)
import Math.Combinat.Partitions

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "15"
    input <- getInput
    print $ maxScore  100 input
    print $ maxScore' 100 input

getInput :: IO [Line]
getInput = readFile "15.txt" >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack

maxScore :: Int -> [Line] -> Int
maxScore l ls = maximum $ map score ds
    where
    ds = 
      [ zip is ls
      | p <- partitionsWithKParts (length ls) l
      , is <- Data.List.permutations $ fromPartition p
      ]
      
maxScore' :: Int -> [Line] -> Int
maxScore' l ls = maximum $ map score ds
    where
    ds = 
      [ ils
      | p <- partitionsWithKParts (length ls) l
      , is <- Data.List.permutations $ fromPartition p
      , let ils = zip is ls
      , sum [i * calories l | (i,l) <- ils] == 500
      ]

-- input <- readFile "15s.txt" >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
-- score [(44,head input),(56,last input)]
score :: [(Int,Line)] -> Int
score ls = product
    [ sum [i*f l | (i,l) <- ls] `max` 0
    | f <- [capacity,durability,flavor,texture]
    ]


type Name = Text
data Line = Line
   { name       :: !Name
   , capacity   :: {-# unpack #-} !Int
   , durability :: {-# unpack #-} !Int
   , flavor     :: {-# unpack #-} !Int
   , texture    :: {-# unpack #-} !Int
   , calories   :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" 

parseLine :: Parser Line
parseLine = do
    -- Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
    name <- word
    ": capacity "
    capacity <- signed decimal
    ", durability "
    durability <- signed decimal
    ", flavor "
    flavor <- signed decimal
    ", texture "
    texture <- signed decimal
    ", calories "
    calories <- signed decimal
    return Line{..}
    where
    word = T.pack <$> many1' letter