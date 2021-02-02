{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Bits
import Data.Char
import Data.Foldable
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
    s <- readFile "14.txt"
    input <- either fail pure $ parseOnly parseLines $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Integer
part1 = sum . _mem . foldl step (Stack undefined mempty)

type Line = Either Mask (Int, Integer)
type Mask = [(Int,Maybe Bool)]
type Mem = Map Int Integer

data Stack = Stack 
   { _mask :: Mask
   , _mem :: Mem
   } deriving (Show, Eq)

step :: Stack -> Line -> Stack
step stack = \case
    Left m -> stack & mask .~ m
    Right (reg,n) -> stack & mem %~ Map.insert reg (applyMask (_mask stack) n)

applyMask :: Mask -> Integer -> Integer
applyMask = flip (foldr $ uncurry go)
    where
    go bit mb = case mb of
        Nothing -> id
        Just b -> flip (if b then setBit else clearBit) bit

type Mask' = (Int, [Int])
fromMask :: Mask -> Mask'
fromMask mask = (foldl go 0 mask, map fst $ filter (isNothing . snd) mask)
    where go n (i,mb) = if mb == Just True then setBit n i else n
data Stack' = Stack' 
   { _mask' :: Mask'
   , _mem' :: Mem
   } deriving (Show, Eq)
part2 :: [Line] -> Integer
part2 = sum . _mem' . foldl step' (Stack' undefined mempty)

step' :: Stack' -> Line -> Stack'
step' stack = \case
    Left m -> stack & mask' .~ fromMask m
    Right (reg,n) -> stack & mem' %~ change n (applyMask' (_mask' stack) reg)
    where
    change n regs mem = foldl (\m reg -> Map.insert reg n m) mem regs 

applyMask' :: Mask' -> Int -> [Int]
applyMask' (i,is) n = foldl go [n .|. i] is  
    where
    go is bit = foldMap (\n -> [clearBit n bit, setBit n bit]) is

-- Parsing
parseLines :: P.Parser [Line]
parseLines = (eitherP parseMask parseRegister `sepBy1'` endOfLine) <* skipSpace <* endOfInput

parseRegister :: P.Parser (Int, Integer)
parseRegister = do
    "mem["
    reg <- decimal
    "] = "
    n <- decimal
    return (reg, n)

parseMask :: P.Parser Mask
parseMask = fin <$> do
    "mask = "
    mask <- P.take 36
    return mask
    where
    fin = zip (reverse [0..35])
        . map (\c -> if c `elem` ("01"::String) then Just (c == '1') else Nothing)
        . T.unpack

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Stack
mask :: Lens' Stack Mask
mask f_amcF (Stack x1_amcG x2_amcH)
    = (fmap (\ y1_amcI -> (Stack y1_amcI) x2_amcH)) (f_amcF x1_amcG)
{-# INLINE mask #-}
mem :: Lens' Stack (Map Int Integer)
mem f_amcJ (Stack x1_amcK x2_amcL)
    = (fmap (\ y1_amcM -> (Stack x1_amcK) y1_amcM)) (f_amcJ x2_amcL)
{-# INLINE mem #-}

mask' :: Lens' Stack' Mask'
mask' f_amcF (Stack' x1_amcG x2_amcH)
    = (fmap (\ y1_amcI -> (Stack' y1_amcI) x2_amcH)) (f_amcF x1_amcG)
{-# INLINE mask' #-}
mem' :: Lens' Stack' (Map Int Integer)
mem' f_amcJ (Stack' x1_amcK x2_amcL)
    = (fmap (\ y1_amcM -> (Stack' x1_amcK) y1_amcM)) (f_amcJ x2_amcL)
{-# INLINE mem' #-}