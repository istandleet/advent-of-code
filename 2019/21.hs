{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language RankNTypes #-}
{-# language NoMonomorphismRestriction #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
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
    putStrLn $ runSpringbot True dat p1Program -- 19358416
    putStrLn $ runSpringbot False dat p2Program 
    
data Register = A | B | C | D | E | F | G | H | I | T | J deriving (Show, Eq, Ord, Enum, Bounded)
data Instruction = 
     AND Register Register
   | OR  Register Register
   | NOT Register Register
   deriving (Show, Eq, Ord)
type Springbot = [Instruction]
    
runSpringbot :: Bool -> Program -> Springbot -> String
runSpringbot p1 prog ins = evalState repl (initComputer prog)
    where
    repl = do
        interactAscii ""
        interactAscii $ unlines $ map show ins ++ [if p1 then "WALK" else "RUN"]
        
p1Program = -- jumps if a, b, or c are open, but only if d is solid.
    [ NOT A J
    , NOT B T -- apparently you can erase this check? tight
    , OR  T J -- apparently you can erase this check? tight
    , NOT C T
    , OR  T J
    , AND D J
    ]
    
p2Program = 
    [ NOT C T, NOT B J, OR J T, AND B J -- T to true if either B or C is empty. J is false from (NOT B J, AND B J)
    , OR  E J, AND I J -- If I can land and take a step then jump
    , OR  H J -- If I can land in jump
    , AND T J
    ] ++
    [ AND D J -- This ending ensures we never jump if D is empty and always jump if A is open
    , NOT A T -- This ending ensures we never jump if D is empty and always jump if A is open
    , OR  T J -- This ending ensures we never jump if D is empty and always jump if A is open
    ]

allInstructions :: [Instruction]
allInstructions =
    [ c r w
    | c <- [AND,OR]
    , r <- [minBound..maxBound]
    , w <- [T,J]
    , r/=w -- OR a a = a; AND a a = a
    ] ++ 
    [ NOT r w
    | r <- [minBound..maxBound]
    , w <- [T,J] -- NOT a a /= a
    ] 
    
allSpringbots :: Int -> [Springbot]
allSpringbots 0 = []
allSpringbots 1 = [[c r J] | c <- [AND,OR,NOT], r <- [minBound..maxBound]] -- The last instruction should target the jump move
allSpringbots n = allSpringbots (n-1) ++ 
    [ i:s
    | s <- allSpringbots (n-1) 
    , i <- allInstructions
    ]
    
-- * Testing
type Registers = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool
                 ,Bool,Bool)
toLens :: Register -> Lens' Registers Bool
toLens = \case
    A -> _1
    B -> _2
    C -> _3
    D -> _4
    E -> _5
    F -> _6
    G -> _7
    H -> _8
    I -> _9
    T -> _10
    J -> _11
runInstructions :: Registers -> Springbot -> Registers
runInstructions = foldl runInstruction
runInstruction :: Registers -> Instruction -> Registers
runInstruction rs = \case
    NOT a b -> rs & toLens b .~ not (rs ^. toLens a)
    AND a b -> rs & toLens b %~ (&& (rs ^. toLens a))
    OR  a b -> rs & toLens b %~ (|| (rs ^. toLens a))
    
testInstructions1 :: Springbot -> [Registers]
testInstructions1 is = do
    d <- [True,False]
    a <- [True,False]
    b <- [True,False]
    c <- [True,False]
    pure $ runInstructions (a,b,c,d,True,True,True,True,True,False,False) is
    
drawInstructions1 = putStrLn . unlines . map draw . testInstructions1
    where draw (a,b,c,d,_,_,_,_,_,t,j) = map (\b -> if b then '#' else '.') [a,b,c,d] ++ show (t,j)
    
testInstructions2 :: Springbot -> [Registers]
testInstructions2 is = map (`runInstructions` is) $ filter (wouldFail is) possibleRegisters2

wouldFail :: Springbot -> Registers -> Bool
wouldFail is rs = 
             if allgood then False
        else if simpleFailure is rs then True
        else wouldFail is nextstep
    where
    allgood = rs == takeStep rs 
    wejump = runInstructions rs is ^. toLens J
    nextstep = if wejump then performJump rs else takeStep rs
    
takeStep :: Registers -> Registers
takeStep (a,b,c,d,e,f,g,h,i,t,j) = (b,c,d,e,f,g,h,i,True,t,j)
performJump :: Registers -> Registers
performJump (a,b,c,d,e,f,g,h,i,t,j) = (e,f,g,h,i,True,True,True,True,t,j)

simpleFailure is rs = 
       (not (rs ^. toLens A) && not wejump) 
    || (wejump && not (rs ^. toLens D))
    where wejump = runInstructions rs is ^. toLens J
    
possibleRegisters2 :: [Registers]
possibleRegisters2 = filter isPossible allRegisters2
isPossible :: Registers -> Bool
isPossible rs = allgood || stepworks || jumpworks
    where
    allgood = rs == takeStep rs 
    stepworks = (rs ^. toLens A) && isPossible (takeStep rs)
    jumpworks = (rs ^. toLens D) && isPossible (performJump rs)
    
allRegisters2 :: [Registers]
allRegisters2 = 
    [ (a,b,c,d,e,f,g,h,i,False,False)
    | a <- [True,False]
    , b <- [True,False]
    , c <- [True,False]
    , d <- [True,False]
    -- , not (a && b && c)
    , e <- [True,False]
    , f <- [True,False]
    , g <- [True,False]
    , h <- [True,False]
    , i <- [True,False]
    ]
    
drawInstructions2 :: Springbot -> IO ()
drawInstructions2 = putStrLn . unlines . map draw . testInstructions2
draw (a,b,c,d,e,f,g,h,i,t,j) = map (\b -> if b then '#' else '.') [True,a,b,c,d,e,f,g,h,i] ++ show (t,j)
    
fromTuple (a,b,c,d,e,f,g,h,i,t,j) = [a,b,c,d,e,f,g,h,i,t,j]
toTuple   [a,b,c,d,e,f,g,h,i,t,j] = (a,b,c,d,e,f,g,h,i,t,j)
fromLine = toTuple . (++[False,False]) . map (=='#') 