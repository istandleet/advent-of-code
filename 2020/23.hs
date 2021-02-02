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
import Data.Time
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
    let input = 135468729
    print $ part1 input -- = 32897654s
    print $ part2UV 10000000 input
    print $ part2UV 10000000 389125467

part1 :: Int -> Int
part1 = fin . (!! 100) . iterate step . Seq.fromList . toDigits
    where 
    fin ans = 
        let Just ix = Seq.elemIndexL 1 ans
            (a,_ :<| b) = Seq.splitAt ix ans
         in foldl1 (\acc i -> acc*10+i) (b <> a)

-- take 20 (part2Cheating $ mkPart2 389125467) == take 20 (keepItGoing' $ mkPart2 389125467)
-- take 20 (part2Cheating $ mkPart2 389125467) == take 20 (iterate step $ mkPart2 389125467)
part2 :: Int -> Int -> Integer
part2 iterations = finPart2 . (!! iterations) . part2Cheating . mkPart2

finPart2 ans = 
    let Just ix = Seq.elemIndexL 1 ans
        (as,_ :<| bs) = Seq.splitAt ix ans
        a :<| b :<| _ = bs <> as
     in toInteger a * toInteger b

mkPart2 :: Int -> Circle
mkPart2 = Seq.fromList . (\ls -> ls++[maximum ls + 1..1000000]) . toDigits

part2Cheating :: Circle -> [Circle]
part2Cheating start = 
        initialmoves
     ++ easymoves
     ++ waves
     ++ tail (keepItGoing' $ last waves)
    where
    initialmoves = takeWhile ((<=11) . flip Seq.index 0) $ iterate step start
    last_slow = step $ last initialmoves
    easymoves = keepItGoing last_slow [start,start+delta..] 
        where 
        start = splitPoint last_slow
        delta = splitPoint (step last_slow) - start
    transition = step $ last easymoves
    waves = 
        let firstdown = keepItGoing transition $ iterate pred $ splitPoint transition
            firstup = step $ last firstdown
         in firstdown ++ keepItGoing firstup (tail $ foldMap (\ix -> [ix, ix-1, ix-2]) [splitPoint firstup+1..])


keepItGoing :: Circle -> [Int] -> [Circle]
keepItGoing c (guess:guesses) = c : case stepEasy guess c of
    Nothing -> []
    Just c' -> keepItGoing c' guesses

keepItGoing' :: Circle -> [Circle]
keepItGoing' seed = seed : go [splitPoint seed] seed
    where
    go mem c = case listToMaybe $ mapMaybe (\ix -> trySeed ix c) mem of
        Just c' -> c' : go mem c'
        Nothing -> 
            let c' = step c
                ix = splitPoint c'
                mem' = if any (\ix' -> abs (ix - ix') <= bubble) mem
                        then mem else ix : take 9 mem 
             in c' : go mem' c'
    trySeed ix c = listToMaybe $ mapMaybe (\d -> stepEasy (d+ix) c) $ deltas 
    deltas = 0 : foldMap (\i -> [i,negate i]) [1..bubble]
    bubble = 5

type Circle = Seq Int
step :: Circle -> Circle
step (curr :<| circle) = 
    let (moved, remaining) = Seq.splitAt 3 circle
        -- ix = fst $ Seq.foldlWithIndex go (0, curr) remaining
        ix = succ $ case listToMaybe $ mapMaybe (`Seq.elemIndexR` remaining) [curr-1,curr-2..1] of
            Just i -> i
            Nothing -> fromJust $ Seq.elemIndexR (maximum remaining) remaining
        (as,bs) = Seq.splitAt ix remaining
     in as <> moved <> bs :|> curr

stepEasy :: Int -> Circle -> Maybe Circle
stepEasy guess (curr :<| circle) = 
    if Seq.lookup guess remaining == Just (curr - 1) 
        then Just (as <> moved <> bs :|> curr)
        else Nothing
    where
    (moved, remaining) = Seq.splitAt 3 circle
    (as,bs) = Seq.splitAt (guess+1) remaining

splitPoint :: Circle -> Int
splitPoint (curr :<| circle) = case mtarget of
    Just t -> fromJust $ Seq.elemIndexR t remaining
    Nothing -> fromJust $ Seq.elemIndexR (maximum remaining) remaining
    where
    (moved, remaining) = Seq.splitAt 3 circle
    mtarget = listToMaybe $ filter (not . isJust . (`Seq.elemIndexR` moved)) [curr-1,curr-2..1]

type Circle' = UV.Vector Int
step' :: Circle' -> Circle'
step' circle = 
    let (moved', remaining) = UV.splitAt 4 circle
        curr = UV.head moved'
        moved = UV.tail moved'
        -- ix = fst $ Seq.foldlWithIndex go (0, curr) remaining
        ix = succ $ case listToMaybe $ mapMaybe (`UV.elemIndex` remaining) [curr-1,curr-2..1] of
            Just i -> i
            Nothing -> fromJust $ UV.elemIndex (UV.maximum remaining) remaining
        (as,bs) = UV.splitAt ix remaining
     in as <> moved <> bs `UV.snoc` curr

stepEasy' :: Int -> Circle' -> Maybe Circle'
stepEasy' guess circle = 
    if remaining UV.!? guess == Just (curr - 1) 
        then Just (as <> moved <> bs `UV.snoc` curr)
        else Nothing
    where
    (moved', remaining) = UV.splitAt 4 circle
    curr = UV.head moved'
    moved = UV.tail moved'
    (as,bs) = UV.splitAt (guess+1) remaining

splitPoint' :: Circle' -> Int
splitPoint' circle = case mtarget of
    Just t -> fromJust $ UV.elemIndex t remaining
    Nothing -> fromJust $ UV.elemIndex (UV.maximum remaining) remaining
    where
    (moved', remaining) = UV.splitAt 4 circle
    curr = UV.head moved'
    moved = UV.tail moved'
    mtarget = listToMaybe $ filter (`UV.elem` moved) [curr-1,curr-2..1]

keepItGoingUV :: Circle' -> [Circle']
keepItGoingUV seed = seed : go [splitPoint' seed] seed
    where
    go mem c = case listToMaybe $ mapMaybe (\ix -> trySeed ix c) mem of
        Just c' -> c' : go mem c'
        Nothing -> 
            let c' = step' c
                ix = splitPoint' c'
                mem' = if any (\ix' -> abs (ix - ix') <= bubble) mem
                        then mem else ix : take 9 mem 
             in c' : go mem' c'
    trySeed ix c = listToMaybe $ mapMaybe (\d -> stepEasy' (d+ix) c) $ deltas 
    deltas = 0 : foldMap (\i -> [i,negate i]) [1..bubble]
    bubble = 5

part2UV :: Int -> Int -> Integer
part2UV iterations = finPart2 . (!! succ iterations) . keepItGoingUV . mkPart2
    where
    finPart2 ans = 
        let Just ix = UV.elemIndex 1 ans
            a = (ans UV.!) . (flip mod (UV.length ans)) $ (ix+1)
            b = (ans UV.!) . (flip mod (UV.length ans)) $ (ix+2)
         in toInteger a * toInteger b

    mkPart2 = UV.fromList . (\ls -> ls++[maximum ls + 1..1000000]) . toDigits

-- * Util
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)

toDigits :: Int -> [Int]
toDigits = reverse . toDigits'

toDigits' :: Int -> [Int]
toDigits' 0 = []
toDigits' n = 
    let (q,r) = n `divMod` 10
     in r : toDigits' q
