{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Foldable
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (the, groupWith)
import Data.Matrix hiding ((<|>))

import Data.Char
import Data.Attoparsec.Text hiding (D, count)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "16"
    ls <- getInput
    putStrLn $ part1 ls ['a'..'p']
    putStrLn $ part2 ls (10^9)
    
getInput :: IO [Line]
getInput = readFile "16.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
part1 :: [Line] -> String -> String
part1 ls = Vector.toList . go ls . Vector.fromList
    where
    go [] a = a
    go (m:ms) a = go ms (move m a)
    
part2 :: [Line] -> Int -> String
part2 ls n = 
    let ss = iterate (part1 ls) ['a'..'p']
        Just i = fmap succ $ Data.List.findIndex (== ['a'..'p']) $ tail ss
        d = n `mod` i
     in ss !! d
      
move :: Line -> Vector Char -> Vector Char
move (Spin i) v = 
    let n = Vector.length v 
        (a,b) = Vector.splitAt (n-i) v
     in b Vector.++ a
move (Exchange i j) v = v Vector.// [(i,v Vector.! j),(j,v Vector.! i)]
move (Partner  a b) v = Vector.map go v
    where go c = if a == c then b else if b == c then a else c
      
data Line = 
     Spin Int
   | Exchange Int Int
   | Partner Char Char
   deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine ","

parseLine :: Parser Line
parseLine = pS <|> pE <|> pP
    where
    pS = do
        char 's'
        Spin <$> decimal
    pE = do
        char 'x'
        a <- decimal
        char '/'
        b <- decimal
        return $ Exchange a b
    pP = do
        char 'p'
        a <- anyChar
        char '/'
        b <- anyChar
        return $ Partner a b
    
toDoubling :: Int -> [Bool]
toDoubling 0 = []
toDoubling n = odd n : toDoubling (n `div` 2)
    
matExp :: Matrix Int -> Int -> Matrix Int
matExp m n = fst $ foldl' go (identity $ nrows m,m) $ toDoubling n
    where
    go (!a,!c) b = (if b then c*a else a,c*c)
    
afterOne :: [Int]
afterOne =  catMaybes $ map (\c -> Data.List.findIndex (==c) ['a'..'p']) afterOneS
afterOneS :: String
afterOneS = "pkgnhomelfdibjac"

afterOneM :: Matrix Int
afterOneM = matrix 16 16 $ \(x,y) -> if afterOne !! (x-1) == (y-1) then 1 else 0

buildString :: Matrix Int -> String
buildString m = map go [1..16]
    where
    s = ['a'..'p']
    go x = s !! (head (filter (\y -> m ! (x,y) > 0) [1..16]) - 1)