{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
{-# language BangPatterns #-}
module Main where 

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.Maybe
import Data.Either
import Text.Read (readMaybe)
import qualified Data.Set as Set

main = do
    putStrLn "23"
    ops <- map readInstruction . lines <$> readFile "23.txt"
    print $ part1 ops
    print $ part2 ops

part1 :: [Instruction] -> OpState
part1 is = execState go (0,0,(0,0,0,0,0,0,0,0))
    where
    go = do
        i <- gets (^. _1)
        unless (i >= length is || i < 0) $ do
            modify' $ runop $ is !! i
            go
part2 :: [Instruction] -> OpState
part2 is = execState go (0,0,(1,0,0,0,0,0,0,0))
    where
    go = do
        i <- gets (^. _1)
        unless (i >= length is || i < 0) $ do
            modify' $ runop $ is !! i
            go
part2' :: [Instruction] -> IO ()
part2' is = evalStateT go (0,0,(1,0,0,0,0,0,0,0))
    where
    go :: StateT OpState IO ()
    go = do
        i <- gets (^. _1) :: StateT OpState IO Int
        unless (i >= length is || i < 0) $ do
            modify' $ runop $ is !! i :: StateT OpState IO ()
            -- m <- gets (^. _2)
            -- when (m `mod` 100000 == 0) $
            --     get >>= liftIO . print 
            go
            
data OpCode = 
     Set
   | Sub
   | Mul
   | Jnz
   deriving (Show, Eq, Ord, Enum, Bounded)

type Registers = (Int,Int,Int,Int,Int,Int,Int,Int)
type OpState = (Int,Int,Registers)
type Val = Either Char Int
type Instruction = (OpCode,Val,Val)

readOpCode :: String -> OpCode
readOpCode "set" = Set
readOpCode "sub" = Sub
readOpCode "mul" = Mul
readOpCode "jnz" = Jnz

readVal :: String -> Val
readVal s = maybe (Left $ head s) Right $ readMaybe s

readInstruction :: String -> Instruction
readInstruction s = let [op,a,b] = words s 
    in (readOpCode op, readVal a, readVal b)

   
runop :: Instruction -> OpState -> OpState
runop (op,a,b) (!i,!ms,!r) = case op of 
    Set -> (succ i,ms,r & l (fl a) .~ getVal b)
    Sub -> (succ i,ms,r & l (fl a) %~ (subtract (getVal b) $!))
    Mul -> (succ i,succ ms,r & l (fl a) %~ (\n -> n * getVal b))
    Jnz -> (if getVal a == 0 then succ i else i + getVal b,ms,r)
    where
    getVal = either (\c -> r ^. l c) id
    fl (Left c) = c
    l = \case
        'a' -> _1
        'b' -> _2
        'c' -> _3
        'd' -> _4
        'e' -> _5
        'f' -> _6
        'g' -> _7
        'h' -> _8