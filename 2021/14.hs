-- See https://hackmd.io/@rkennedy/r11t5dI5Y

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
import Data.Attoparsec.Text as P hiding (takeWhile, take, count)

import Data.Matrix as M

main :: IO ()
main = do
    input <- getInput <$> readFile "14x.txt"
    let (s, rs) = fmap Map.fromList input
    interact $ show . (`mod` 10^20) . solution rs s . read
--     print $ part1 input
--     print $ part2 s rs

-- part1 :: Input -> Int
part1 (s,rs) = go $ (!!10) $ iterate (step $ Map.fromList rs) s 
    where 
    go = diff . map snd . count
    -- diff ls = ls
    diff ls = maximum ls - minimum ls
part2 s rs = solution rs s 40

solution :: Rules -> String -> Int -> Integer
solution rs s d = diff $ map snd $ charCounts rs s d
    where diff ls = maximum ls - minimum ls

charCounts :: Rules -> String -> Int -> [(Char, Integer)]
charCounts rs s d = fixLast $ fromVec' cmap $ toVec cmap s * matExp (adjacency cmap rs) d
    where 
    cmap = mkCMap s rs
    fixLast = map (\(c,a) -> (c, if c == last s then a+1 else a))

matExp :: Matrix Integer -> Int -> Matrix Integer
matExp m n = fst $ foldl' go (identity $ nrows m,m) $ toDoubling n
    where
    go (!a,!c) !b = (if b then c*a else a,c*c)
    
    toDoubling 0 = []
    toDoubling n = odd n : toDoubling (n `div` 2)

count ts = [(the t,length t)|t <- ts, then group by t using groupWith]


toVec :: CMap -> String -> Matrix Integer
toVec cmap s = M.fromList 1 (l*l) $ map f [0..l*l-1]
  where
  !l = length cmap
  fromIx i = bimap (cmap V.!) (cmap V.!) $ i `divMod` l
  cs = count $ zip s (tail s)
  f i = let (a,b) = fromIx i in maybe 0 fromIntegral $ lookup (a,b) cs

fromVec :: CMap -> Matrix Integer -> [((Char,Char), Integer)]
fromVec cmap m =
    [ ((a,b),c)
    | a <- V.toList cmap
    , b <- V.toList cmap
    , let x = toIx a b
          c = m M.! (1,x)
    , c > 0
    ]
    where
    !l = length cmap
    toIx a b = fromJust (V.elemIndex a cmap) * l + fromJust (V.elemIndex b cmap) + 1

fromVec' :: CMap -> Matrix Integer -> [(Char, Integer)]
fromVec' cmap m =
    [ (the a,sum c)
    | a <- V.toList cmap
    , b <- V.toList cmap
    , let x = toIx a b
          c = m M.! (1,x)
    , then group by a using groupWith
    ]
    where
    !l = length cmap
    toIx a b = fromJust (V.elemIndex a cmap) * l + fromJust (V.elemIndex b cmap) + 1

type CMap = V.Vector Char
mkCMap :: String -> Rules -> CMap
mkCMap s rs = V.fromList $ Data.List.sort $ Set.toList allchars
  where
  allchars = Set.fromList (Map.elems rs)
          <> Set.map fst (Map.keysSet rs)
          <> Set.map snd (Map.keysSet rs)
          <> Set.fromList s

adjacency :: CMap -> Rules -> Matrix Integer
adjacency cmap rs = M.matrix (l*l) (l*l) $ uncurry f . bimap pred pred
  where
  !l = length cmap
  fromIx i = bimap (cmap V.!) (cmap V.!) $ i `divMod` l
--   f :: Int -> Int -> Int
  f i = case Map.lookup (a, b) rs of
      Nothing -> \j -> if j == i then 1 else 0 -- if there isn't a rule they map to themselves
      Just c -> \j -> if fromIx j `elem` [(a,c),(c,b)] then 1 else 0
    where 
    (a,b) = fromIx i

toRules :: CMap -> Matrix Integer -> [Rule]
toRules cmap m = 
    [ ((a,b),c)
    | a <- V.toList cmap
    , b <- V.toList cmap
    , c <- V.toList cmap
    , let x = toIx a b
          i = toIx a c 
          j = toIx c b
    , m M.! (x,i) > 0 && m M.! (x,j) > 0
    ]
    where
    !l = length cmap
    toIx a b = fromJust (V.elemIndex a cmap) * l + fromJust (V.elemIndex b cmap) + 1

type St = String
type Rule = ((Char, Char),Char)
type Rules = Map (Char,Char) Char
type Input =  (St, [Rule])
step :: Rules -> St -> St
step rs = \s -> (head s :) $ foldMap (uncurry go) $ zip s (tail s)
    where
    go :: Char -> Char -> String
    go a b = case Map.lookup (a,b) rs of 
        Nothing -> [b]
        Just c -> [c,b]

type Line = (Text, Text)
getInput :: String -> Input
getInput = (\(a:s) -> (a, map p s)) . filter (not . null) . lines 
    where 
    go = fmap tail . break (=='-')
    p [a,b,_,_,_,_,c] = ((a,b),c)
