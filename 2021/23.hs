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
import qualified Data.Sequence as Seq
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, count)

import Data.Time

main :: IO ()
main = do
    getCurrentTime >>= print
    print $ solve example
    getCurrentTime >>= print
    print $ solve input
    getCurrentTime >>= print
    print $ solve example'
    getCurrentTime >>= print
    print $ solve input'
    getCurrentTime >>= print

part1, part2 :: Input -> Int
part1 = solve
part2 = solve

type Input = (String, String, String, String)
type Board = (Map Int Char, Int, Input)

solve :: Input -> Int
solve (as, bs, cd, ds) = go Nothing [initialize (as, bs, cd, ds)]
    where
    depth = length as
    go minscore [] = fromJust minscore
    go minscore ls = 
        let (done,cont) = Data.List.partition solved $ foldMap (moves depth) ls
            minthis = if null done then minscore else let t = minimum $ map (\(_,s,_) -> s) done in if isNothing minscore then Just t else min minscore (Just t)
            mincont = 
                [ (the m, minimum s, the r) 
                | (m,s,r) <- cont
                , then group by (m,r) using groupWith
                ]
         in go minthis $ case minthis of 
                Nothing -> mincont 
                Just t -> filter (\(_,s,_) -> s < t) mincont

solved :: Board -> Bool
solved (m,_,(as, bs, cs, ds)) = 
    null m && all (=='A') as && all (=='B') bs && all (=='C') cs && all (=='D') ds

showBoard (hall, score, (as,bs,cs,ds)) = unlines
    [ show score
    , replicate 13 '#'
    , "#" <> (map (\i -> maybe '.' id $ Map.lookup i hall) [0..10]) <> "#"
    , "###" <> t as <> "#" <> t bs <> "#" <> t cs <> "#" <> t ds <> "###"
    , "  #" <> b as <> "#" <> b bs <> "#" <> b cs <> "#" <> b ds <> "#"
    , "  #########"
    ]
    where 
    t xs = [if length xs == 2 then head xs else '.']
    b xs = [if null xs then '.' else last xs]

initialize :: Input -> Board
initialize = (,,) mempty 0

input   = ("DB","AC","CB","DA")
example = ("BA","CD","BC","DA")

input'   = ("DDDB","ACBC","CBAB","DACA")
example' = ("BDDA","CCBD","BBAC","DACA")

moves :: Int -> Board -> [Board]
moves depth (hall, score, inputs) = if not $ null movehall then movehall else move1 <> move2 <> move3 <> move4
    where
    (as,bs,cs,ds) = inputs
    move1 = case as of
        xs | all (=='A') xs -> []
        (x:xs) -> map (\i -> (Map.insert i x hall, score + (abs(2-i)+depth-length xs)*scoring x, (xs,bs,cs,ds)))
                 $ filter (\i -> null $ occupied `Set.intersection` quickrange i 2) endingpos 
    move2 = case bs of 
        xs | all (=='B') xs -> []
        (x:xs) -> map (\i -> (Map.insert i x hall, score + (abs(4-i)+depth-length xs)*scoring x, (as,xs,cs,ds)))
                 $ filter (\i -> null $ occupied `Set.intersection` quickrange i 4) endingpos 
    move3 = case cs of 
        xs | all (=='C') xs -> []
        (x:xs) -> map (\i -> (Map.insert i x hall, score + (abs(6-i)+depth-length xs)*scoring x, (as,bs,xs,ds)))
                 $ filter (\i -> null $ occupied `Set.intersection` quickrange i 6) endingpos  
    move4 = case ds of 
        xs | all (=='D') xs -> []
        (x:xs) -> map (\i -> (Map.insert i x hall, score + (abs(8-i)+depth-length xs)*scoring x, (as,bs,cs,xs)))
                 $ filter (\i -> null $ occupied `Set.intersection` quickrange i 8) endingpos
    movehall = flip mapMaybe endingpos $ \i -> Map.lookup i hall >>= \x -> case x of 
        'A' -> if not $ all (==x) as && (null $ Set.delete i occupied `Set.intersection` quickrange i 2) then Nothing 
                else Just (Map.delete i hall, score + moveinto x i, (x:as,bs,cs,ds))
        'B' -> if not $ all (==x) bs && (null $ Set.delete i occupied `Set.intersection` quickrange i 4) then Nothing 
                else Just (Map.delete i hall, score + moveinto x i, (as,x:bs,cs,ds))
        'C' -> if not $ all (==x) cs && (null $ Set.delete i occupied `Set.intersection` quickrange i 6) then Nothing 
                else Just (Map.delete i hall, score + moveinto x i, (as,bs,x:cs,ds))
        'D' -> if not $ all (==x) ds && (null $ Set.delete i occupied `Set.intersection` quickrange i 8) then Nothing 
                else Just (Map.delete i hall, score + moveinto x i, (as,bs,cs,x:ds))

    moveinto x i = case x of 
        'A' -> ((depth-length as)+abs(2-i))*scoring x
        'B' -> ((depth-length bs)+abs(4-i))*scoring x
        'C' -> ((depth-length cs)+abs(6-i))*scoring x
        'D' -> ((depth-length ds)+abs(8-i))*scoring x

    occupied = Map.keysSet hall

quickrange i j = Set.fromList [min i j .. max i j]

endingpos = [0,1,3,5,7,9,10]

scoring :: Char -> Int
scoring = \case 
    'A' -> 1
    'B' -> 10
    'C' -> 100
    'D' -> 1000
