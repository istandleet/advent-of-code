{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, count, Done)

import Data.Time

main :: IO ()
main = do
    input <- getInput
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print
    print $ part2 input
    getCurrentTime >>= print

part1, part2 :: Input -> Integer
part1 = magnitude . foldl1 add
part2 is = maximum [go a b | a <- is, b <- is, a /= b]
    where go a b = magnitude $ add a b

add :: Integral a => Snail a -> Snail a -> Snail a
add a b = last $ iterateMaybe step $ Branch a b

magnitude = \case
    Leaf a -> a 
    Branch a b -> 3 * magnitude a + 2 * magnitude b

step snail = case reduce 0 snail of 
    Exploded a -> Just a 
    Split a -> Just a 
    ExplodeL _ a -> Just a
    ExplodeR a _ -> Just a
    None -> Nothing 

data Reduction a = 
      Explode a a 
    | ExplodeL a (Snail a) 
    | ExplodeR (Snail a) a
    | Exploded (Snail a)
    | Split (Snail a)
    | None
    deriving (Show, Eq, Functor)

reduce :: (Integral a, Num a) => Int -> Snail a -> Reduction a
reduce _ (Leaf a)
    | a > 9 = let (q,r) = a `divMod` 2 in Split $ Branch (Leaf q) (Leaf (q+r))
    | otherwise = None
reduce n (Branch (Leaf a) (Leaf b)) | n >= 4 = Explode a b
reduce n (Branch a b) = case reduce (succ n) a of
    Explode  x y -> ExplodeL x (Branch (Leaf 0) (addL y b))
    ExplodeL x y -> ExplodeL x (Branch y b)
    ExplodeR x y -> Exploded $ Branch x (addL y b)
    None -> goright
    Exploded x -> Exploded $ Branch x b
    Split x -> let stay = Split $ Branch x b in case goright of 
        None    -> stay
        Split _ -> stay 
        _ -> goright 
    where
    goright = case reduce (succ n) b of
        Explode  x y -> ExplodeR (Branch (addR a x) (Leaf 0)) y
        ExplodeR x y -> ExplodeR (Branch a x) y
        ExplodeL x y -> Exploded $ Branch (addR a x) y
        Split y -> Split $ Branch a y
        Exploded y -> Exploded $ Branch a y
        None -> None
    
addL :: Num a => a -> Snail a -> Snail a
addL x s = case s of
    Leaf n -> Leaf (x+n)
    Branch a b -> Branch (addL x a) b
addR :: Num a => Snail a -> a -> Snail a
addR s x = case s of 
    Leaf n -> Leaf (x+n)
    Branch a b -> Branch a (addR b x)

type Input = [Snail Integer]
data Snail a = Leaf a | Branch (Snail a) (Snail a) deriving (Eq, Ord, Functor, Foldable)
instance Show a => Show (Snail a) where 
    show (Leaf a) = show a 
    show (Branch a b) = "[" <> show a <> "," <> show b <> "]"

getInput :: IO Input
getInput = readFile "18.txt" >>= either fail return . mapM readLine . filter (not . null) . lines

readLine :: String -> Either String (Snail Integer)
readLine = P.parseOnly (parseSnail <* P.skipSpace <* P.endOfInput) . T.pack
quickread = either error id . readLine

parseSnail = do
    c <- P.anyChar
    case c of 
        '[' -> do 
            a <- parseSnail
            P.char ','
            b <- parseSnail
            P.char ']'
            return $ Branch a b 
        c | c `elem` ['0'..'9'] -> pure $ Leaf $ read [c] 
        otherwise -> fail $ unwords ["Unexpected char", [c]]

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)