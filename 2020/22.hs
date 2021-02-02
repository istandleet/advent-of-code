{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
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
import Data.Attoparsec.Text as P hiding (takeWhile, take)

main :: IO ()
main = do
    s <- readFile "22.txt"
    input <- either fail pure $ parseOnly parseInput $ T.pack s
    print $ part1 input -- 33925
    print $ part2 input

part1 :: St -> Integer
part1 = sum . zipWith (*) [1..] . map toInteger . reverse . toList . winner . last . iterateMaybe step
    where 
    winner st = if not $ null $ fst st then fst st else snd st

step :: St -> Maybe St
step ((a :<| as), (b :<| bs)) = Just $ if a > b then (as :|> a :|> b, bs) else (as, bs :|> b :|> a)
step _ = Nothing

type St = (Hand, Hand)

type Hand = Seq Int
type MemHand = UV.Vector Int
type Memory = Set (MemHand, MemHand)
type Game = (Memory, St)

-- {-
part2 :: St -> Integer
part2 = either fin fin . uncurry (player1winsRC mempty)
    where fin = sum . zipWith (*) [1..] . map toInteger . reverse . toList

player1winsRC :: Memory -> Hand -> Hand -> Either Hand Hand
player1winsRC mem Empty p2 = Right p2
player1winsRC mem p1 Empty = Left p1
player1winsRC mem p1 p2 = 
    if memhand `Set.member` mem
        then Left p1
        else uncurry (player1winsRC mem') $ 
                        if winsThisRound
                            then (as :|> a :|> b, bs)
                            else (as, bs :|> b :|> a)
    where
    memhand = (UV.fromList $ toList p1, UV.fromList $ toList p2) :: (MemHand,MemHand)
    mem' = Set.insert memhand mem
    winsThisRound =
        if length as < a || length bs < b 
            then a > b
            else isLeft $ player1winsRC mempty (Seq.take a as) (Seq.take b bs)
    a :<| as = p1
    b :<| bs = p2
-- -}
{-
part2 :: St -> Integer
part2 = either fin fin . flip evalState mempty . uncurry (player1winsRC' mempty)
    where fin = sum . zipWith (*) [1..] . map toInteger . reverse . toList

type MemGame = State (Map (MemHand,MemHand) Bool)
player1winsRC' :: Memory -> Hand -> Hand -> MemGame (Either Hand Hand)
player1winsRC' mem Empty p2 = pure $ Right p2
player1winsRC' mem p1 Empty = pure $ Left p1
player1winsRC' mem p1 p2 = do
    winsThisRound <- mwinsThisRound
    uncurry (player1winsRC' mem') $ 
                        if winsThisRound
                            then (as :|> a :|> b, bs)
                            else (as, bs :|> b :|> a)
    where
    memhand = (UV.fromList $ toList p1, UV.fromList $ toList p2) :: (MemHand,MemHand)
    mem' = Set.insert memhand mem
    a :<| as = p1
    b :<| bs = p2
    mwinsThisRound = 
        if memhand `Set.member` mem then pure True else
        if length as < a || length bs < b then pure (a > b) else do
            mentry <- Map.lookup memhand <$> get
            case mentry of 
                Just b -> pure b
                Nothing -> do
                    wins <- isLeft <$> player1winsRC' mempty (Seq.take a as) (Seq.take b bs)
                    modify $ Map.insert memhand wins . Map.insert (swap memhand) (not wins) 
                    pure wins
-- -}

-- * Parsing
parseInput :: P.Parser St
parseInput = do
    ("Player 1:" <* P.endOfLine) <?> "p1 header"
    p1 <- (P.decimal `sepBy1'` P.endOfLine) <?> "p1"
    (P.endOfLine <* P.endOfLine) <?> "between"
    ("Player 2:" <* P.endOfLine) <?> "p2 header"
    p2 <- (P.decimal `sepBy1'` P.endOfLine) <?> "p2"
    (P.skipSpace <* P.endOfInput) <?> "end"
    return (Seq.fromList p1,Seq.fromList p2)

-- * util
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)
