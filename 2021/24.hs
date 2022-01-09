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

main = do
    putStrLn "24"
    input <- getInput
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print
    print $ part2 input
    getCurrentTime >>= print

part1 :: Input -> Int
part1 = length
part2 = part1

type Input = [Action]
type Wire = Char
type Val = Either Wire Int
      
data Action = 
    Inp Wire
  | Add Wire Val
  | Mul Wire Val 
  | Div Wire Val 
  | Mod Wire Val 
  | Eql Wire Val
  deriving (Show,Eq)

allCombinations = replicateM 14 $ reverse [1..9]


data St = St 
   { _inputs :: [Int] 
   , _wires  :: Map Wire Int
   } deriving Show
initState = flip St (Map.fromList $ zip ['w'..'z'] $ repeat 0)

exec input = _wires . execState (mapM_ applyAction input) . initState

applyAction :: Action -> State St ()
applyAction = \case
    Inp w   -> inp w   
    Add w v -> add w v 
    Mul w v -> mul w v 
    Div w v -> div' w v 
    Mod w v -> mod' w v 
    Eql w v -> eql w v 

inp c = modify' $ \(St (i:is) d) -> St is $ Map.insert c i d
toTransform f = \w v -> modify' $ lwires %~ \m -> Map.adjust (f $ either (m Map.!) id v) w m
add = toTransform (+)
mul = toTransform (*)
div' = toTransform (flip div)
mod' = toTransform (flip mod)
eql = toTransform (\new old -> if new == old then 1 else 0)


parseAction :: Parser Action
parseAction = choice [inp, add, mul, div, mod, eql] 
    where
    inp = do
        "inp "
        a <- parseWire
        pure $ Inp a
    add = do
        "add "
        a <- parseWire
        " "
        b <- parseVal
        pure $ Add a b
    mul = do
        "mul "
        a <- parseWire
        " "
        b <- parseVal
        pure $ Mul a b
    div = do
        "div "
        a <- parseWire
        " "
        b <- parseVal
        pure $ Div a b
    mod = do
        "mod "
        a <- parseWire
        " "
        b <- parseVal
        pure $ Mod a b
    eql = do
        "eql "
        a <- parseWire
        " "
        b <- parseVal
        pure $ Eql a b
        
parseWire :: Parser Wire
parseWire = letter
parseVal :: Parser Val
parseVal = eitherP parseWire (signed decimal)

getInput :: IO Input
getInput = readFile "24r.txt" >>= either fail return . getInput'
getInput' :: String -> Either String Input
getInput' = P.parseOnly (P.sepBy1' parseAction "\n" <* P.skipSpace <* P.endOfInput) . T.pack

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''St

linputs :: Lens' St [Int]
linputs f_apAh (St x1_apAi x2_apAj)
    = (fmap (\ y1_apAk -> (St y1_apAk) x2_apAj)) (f_apAh x1_apAi)
{-# INLINE linputs #-}
lwires :: Lens' St (Map Wire Int)
lwires f_apAl (St x1_apAm x2_apAn)
    = (fmap (\ y1_apAo -> (St x1_apAm) y1_apAo)) (f_apAl x2_apAn)
{-# INLINE lwires #-}