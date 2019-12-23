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

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "25"
    stars <- getInput
    print $ length $ foldr addStar mempty stars

getInput :: IO [Star]
getInput = readFile "25.txt"
      >>= either fail return . parseOnly parseStars . T.pack
      
distance :: Star -> Star -> Int
distance s1 s2 = 
    abs (a s1 - a s2)
  + abs (b s1 - b s2)
  + abs (c s1 - c s2)
  + abs (d s1 - d s2)

type Constellation = [Star]

addStar :: Star -> [Constellation] -> [Constellation]
addStar s cs = 
    let (hits,misses) = Data.List.partition near cs
     in case hits of 
         [] -> ([s]:cs)
         _  -> ((s:concat hits):misses)
    where
    near = any ((<= 3) . distance s)
      
      
      
data Star = Star
   { a :: {-# unpack #-} !Int
   , b :: {-# unpack #-} !Int
   , c :: {-# unpack #-} !Int
   , d :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseStars :: Parser [Star]
parseStars = sepBy1' parseStar "\n"

parseStar :: Parser Star
parseStar = do
    -- 989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
    a <- signed decimal
    char ','
    b <- signed decimal
    char ','
    c <- signed decimal
    char ','
    d <- signed decimal
    return Star{..}
    
s = "0,0,0,0\n\
\3,0,0,0\n\
\0,3,0,0\n\
\0,0,3,0\n\
\0,0,0,3\n\
\0,0,0,6\n\
\9,0,0,0\n\
\12,0,0,0"