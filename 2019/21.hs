{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language RankNTypes #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.List
import Data.Array.Unboxed

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "21.txt" :: IO Program
    putStrLn $ runSpringboard dat p1Program
    
data Register = A | B | C | D | T | J deriving (Show, Eq, Ord, Enum, Bounded)
data Instruction = 
     AND Register Register
   | OR  Register Register
   | NOT Register Register
   deriving (Show, Eq, Ord)
   
p1Program = -- jumps if a, b, or c are open, but only if d is solid.
    [ NOT A J
    , NOT B T
    , OR  T J
    , NOT C T
    , OR  T J
    , AND D J
    ]
    
compileProgram = (++"WALK\n") . unlines . map show
    
runSpringboard :: Program -> [Instruction] -> String
runSpringboard prog ins = evalState repl (initComputer prog)
    where
    repl = do
        interactAscii ""
        interactAscii $ compileProgram ins
        
-- * Testing
type Registers = (Bool,Bool,Bool,Bool,Bool,Bool)
toLens :: Register -> Lens' Registers Bool
toLens = \case
    A -> _1
    B -> _2
    C -> _3
    D -> _4
    T -> _5
    J -> _6
runInstructions :: Registers -> [Instruction] -> Registers
runInstructions = foldl runInstruction
runInstruction :: Registers -> Instruction -> Registers
runInstruction rs = \case
    NOT a b -> rs & toLens b .~ not (rs ^. toLens a)
    AND a b -> rs & toLens b %~ (&& (rs ^. toLens a))
    OR  a b -> rs & toLens b %~ (|| (rs ^. toLens a))
    
testInstructions :: [Instruction] -> [Registers]
testInstructions is = do
    d <- [True,False]
    a <- [True,False]
    b <- [True,False]
    c <- [True,False]
    pure $ runInstructions (a,b,c,d,False,False) is
    
drawInstructions = putStrLn . unlines . map draw . testInstructions
    where draw (a,b,c,d,t,j) = map (\b -> if b then '#' else '.') [a,b,c,d] ++ show (t,j)