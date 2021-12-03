-- cat 02.txt | runghc 02.hs


{-# language OverloadedStrings #-}
{-# language LambdaCase #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

main :: IO ()
main = do
    s <- getInput
    print $ part1 s
    print $ part2 s

type Coord = (Int, Int)

part1 :: [Line] -> Int
part1 = uncurry (*) . foldl step (0,0)

readints :: String -> [Int]
readints = map read . filter (not.null) . lines

data Line = Down Int | Up Int | Forward Int deriving (Show, Eq)

step :: Coord -> Line -> Coord
step (x,y) = \case 
    Down    n -> (x,y-n)
    Up      n -> (x,y+n)
    Forward n -> (x+n,y)

part2 :: [Line] -> Int
part2 = (\(x,y,a) -> x * y) . foldl step2 (0,0, 0)

step2 :: (Int, Int, Int) -> Line -> (Int, Int, Int) 
step2 (x,y,a) = \case 
    Down    n -> (x,y, a+n)
    Up      n -> (x,y, a-n)
    Forward n -> (x+n,y+a*n,a)

-- * Parsing

getInput :: IO [Line]
getInput = readFile "02.txt" >>= either fail return . parseOnly parseLines . T.pack

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" <* skipSpace <* endOfInput

parseLine :: Parser Line
parseLine = 
        (Down    <$> (P.string "down "    *> P.decimal))
    <|> (Up      <$> (P.string "up "      *> P.decimal))
    <|> (Forward <$> (P.string "forward " *> P.decimal))

-- * util
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)
