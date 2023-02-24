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
    input <- readFile "21.txt"
    let m = Map.fromList $ map readLine $ filter (not . null) $ lines input
        m' = Map.insert "humn" Var m
    print $ part1 m
    print $ part2 m' -- <9993963640759

part1 :: Map String Op -> Int
part1 m = performOp m $ m Map.! "root"

part2 :: Map String Op -> Int
part2 m = case distribute m "root" of
    Plus'  a b -> solve a b
    Minus' a b -> solve a b
    Times' a b -> solve a b
    Div'   a b -> solve a b
    _ -> error "part2"

type Op = Op' String
data Op' a = Number Int | Plus a a | Minus a a | Times a a | Div a a | Var
    deriving (Show, Eq, Ord)

data Oper = Number' Int | Plus' Oper Oper | Minus' Oper Oper | Times' Oper Oper | Div' Oper Oper | Var' -- | Equals Oper Oper
    deriving (Show, Eq, Ord)
--     deriving (Eq, Ord)
-- instance Show Oper where
--     show = \case
--         Number' n -> show n
--         Plus'  a b -> unwords [show a, "+", show b]
--         Minus' a b -> unwords [show a, "-", show b]
--         Times' a b -> unwords [show a, "*", show b]
--         Div'   a b -> unwords [show a, "/", show b]
--         Var' -> "x"
--         -- Equals a b -> unwords [show a, "=", show b]

distribute :: Map String Op -> String -> Oper
distribute m s = case m Map.! s of
    Number n -> Number' n
    Plus  a b -> Plus'  (distribute m a) (distribute m b)
    Minus a b -> Minus' (distribute m a) (distribute m b)
    Times a b -> Times' (distribute m a) (distribute m b)
    Div   a b -> Div'   (distribute m a) (distribute m b)
    Var -> Var'

solve :: Oper -> Oper -> Int
solve a b = let (n, x, _) = path a b in solve' n x
    where
    solve' n = \case
        Plus'  a b -> let (n', x, _) = path a b in solve' (n-n') x
        Minus' a b -> let (n', x, l) = path a b in solve' (if l then n'-n else n+n') x
        Times' a b -> let (n', x, _) = path a b in solve' (n `div` n') x
        Div'   a b -> let (n', x, l) = path a b in solve' (if l then n' `div` n else n*n') x
        Var' -> n
        _ -> error $ unwords ["solve", show a, show b]

path :: Oper -> Oper -> (Int, Oper, Bool)
path a b = case performOper a of 
    Just n -> (n, b, True)
    Nothing -> case performOper b of 
        Just n -> (n, a, False)
        Nothing -> error $ unwords ["path", show a, show b]

performOper :: Oper -> Maybe Int
performOper = \case
    Number' n -> Just n
    Plus'  a b -> liftA2 (+) (performOper a) (performOper b)
    Minus' a b -> liftA2 (-) (performOper a) (performOper b)
    Times' a b -> liftA2 (*) (performOper a) (performOper b)
    Div'   a b -> liftA2 div (performOper a) (performOper b)
    Var' -> Nothing

performOp :: Map String Op -> Op -> Int
performOp m = go
    where 
    go = \case
        Number n -> n
        Plus  a b -> go (m Map.! a) + go (m Map.! b)
        Minus a b -> go (m Map.! a) - go (m Map.! b)
        Times a b -> go (m Map.! a) * go (m Map.! b)
        Div   a b -> go (m Map.! a) `div` go (m Map.! b)
        Var -> error "performOp: Var"

performOp' :: Map String Op -> Op -> Maybe Int
performOp' m = go
    where 
    go = \case
        Number n -> Just n
        Plus  a b -> liftA2 (+) (go $ m Map.! a) (go $ m Map.! b)
        Minus a b -> liftA2 (-) (go $ m Map.! a) (go $ m Map.! b)
        Times a b -> liftA2 (*) (go $ m Map.! a) (go $ m Map.! b)
        Div   a b -> liftA2 div (go $ m Map.! a) (go $ m Map.! b)
        Var -> Nothing

-- * Util
readLine :: String -> (String, Op)
readLine s = case words s of 
    [a, b] -> (init a, Number $ read b)
    [a, b, "+", c] -> (init a, Plus b c)
    [a, b, "-", c] -> (init a, Minus b c)
    [a, b, "*", c] -> (init a, Times b c)
    [a, b, "/", c] -> (init a, Div b c)
    _ -> error $ "readLine: " ++ s
