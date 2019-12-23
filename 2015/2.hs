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
    putStrLn "2"
    dims <- getInput
    print $ sum $ map neededPaper dims
    print $ sum $ map neededRibbon dims

getInput :: IO [Dim]
getInput = readFile "2.txt"
      >>= either fail return . parseOnly parseDims . T.pack
      
neededPaper :: Dim -> Int
neededPaper Dim{..} = 
    (2*l*w + 2*w*h + 2*h*l) + minimum [l*w,w*h,h*l]
    
neededRibbon :: Dim -> Int
neededRibbon Dim{..} = 2*a+2*b+w*l*h
    where
    [a,b] = Data.List.take 2 $ Data.List.sort [w,l,h]
      
data Dim = Dim
   { l :: {-# unpack #-} !Int
   , w :: {-# unpack #-} !Int
   , h :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseDims :: Parser [Dim]
parseDims = sepBy1' parseDim "\n"

parseDim :: Parser Dim
parseDim = do
    -- 989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
    l <- decimal
    char 'x'
    w <- decimal
    char 'x'
    h <- decimal
    return Dim{..}