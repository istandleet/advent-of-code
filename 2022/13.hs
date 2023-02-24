{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take, I)

main :: IO ()
main = do
    input <- readFile "13.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1 :: [(Packet, Packet)] -> Int
part1 cs = sum [i | (i, (a,b)) <- zip [1..] cs, a < b]

part2 :: [(Packet, Packet)] -> Int
part2 cs = go $ Data.List.sort $ (L [L [I 2]]:) $ (L [L [I 6]]:) $ foldMap (\(a,b) -> [a,b]) cs
    where
    go ps = let [a,b] = [i | (i,p) <- zip [1..] ps, p == L [L [I 2]] || p == L [L [I 6]]]
             in a * b

data Packet = L [Packet] | I Int deriving (Show)

instance Eq Packet where 
    a == b = compare a b == EQ
instance Ord Packet where
    compare (I a) (I b) = compare a b
    compare (I a) (L b) = compare (L [I a]) (L b)
    compare (L a) (I b) = compare (L a) (L [I b])
    compare (L []) (L []) = EQ
    compare (L []) (L _) = LT
    compare (L _) (L []) = GT
    compare (L (a:as)) (L (b:bs)) = case compare a b of
        EQ -> compare (L as) (L bs)
        x -> x

-- * Parsing
parseInput :: Parser [(Packet, Packet)] 
parseInput = sepBy1' parsePair skipSpace <* skipSpace <* endOfInput
    where
    parsePair = do
        p1 <- parsePacket
        skipSpace
        p2 <- parsePacket
        return (p1, p2)

parsePacket :: Parser Packet
parsePacket = (L <$> parseList) <|> (I <$> decimal)

parseList :: Parser [Packet]
parseList = char '[' *> sepBy' parsePacket (char ',') <* char ']'
