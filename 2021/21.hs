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
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print
    print $ part2 input
    getCurrentTime >>= print

-- part1, part2 :: Input -> Integer
part1 = go 0 0 0 . tail . moves
    where
    go n a b ((x,y):game) = 
        let a' = a + x 
            b' = b + y
         in      if a' >= 1000 then (b  * (n + 3))
            else if b' >= 1000 then (a' * (n + 6))
            else go (n+6) a' b' game
part2 i = go $ [(fromInput i, 1)]
    where 
    go s = if any (not . isDone . fst) s 
         then go $ step s 
         else max (sum $ map snd $ filter ((>=21) . p1Score . fst) s)
                  (sum $ map snd $ filter ((>=21) . p2Score . fst) s)

fromInput (a, b) = Turn True 0 a 0 b

data Turn = Turn 
   { p1Goes  :: !Bool
   , p1Score :: !Int 
   , p1Pos   :: !Int 
   , p2Score :: !Int 
   , p2Pos   :: !Int
   } deriving (Eq, Ord, Show)

isDone t = p1Score t >= 21 || p2Score t >= 21

step :: [(Turn, Integer)] -> [(Turn, Integer)]
step worlds = 
    [ (the t', sum n )
    | (t, i) <- worlds
    , (t', i') <- evolve t
    , let n = i * i'
    , then group by t' using groupWith
    ]
    where go t n = map (fmap (*n)) $ evolve t

evolve :: Turn -> [(Turn, Integer)]
evolve t | isDone t = [(t,1)]
evolve (Turn True a b c d) =
    [ (newturn, n)
    | (roll, n) <- rolls
    , let newsquare = succ $ (b + roll - 1) `mod` 10
          newturn = Turn False (a + newsquare) newsquare c d 
    ]
evolve (Turn False a b c d) =
    [ (newturn, n)
    | (roll, n) <- rolls
    , let newsquare = succ $ (d + roll - 1) `mod` 10
          newturn = Turn True a b (c + newsquare) newsquare
    ]

rolls :: [(Int,Integer)]
rolls = 
    [ (3, 1)
    , (4, 3)
    , (5, 6)
    , (6, 7)
    , (7, 6)
    , (8, 3)
    , (9, 1)
    ]

moves = go 0 
    where 
    go n (a,b) = 
        let a' = move n a 
            b' = move (n+1) b 
         in (a, b) : go (n+2) (a', b')

move n space = 
    let die = map roll $ take 3 [n*3..]
     in succ $ (space + sum die - 1) `mod` 10
    where 
    roll n = (n `mod` 100) + 1

type Input = (Int, Int)

input, example :: Input
input = (9,4)
example = (4,8)