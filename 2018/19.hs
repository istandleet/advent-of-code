module Main where 

import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.Set as Set

main = do
    s <- readFile "19.txt"
    let (ip:ops) = lines s
    print $ part1 $ map readInstruction ops
    print $ part2 $ map readInstruction ops

runOp :: Instruction -> Registers -> Registers
runOp (op,a,b,c) = runop op a b c

part1 :: [Instruction] -> Registers
part1 is = execState go (0,0,0,0,0,0)
    where
    go = do
        i <- gets (^. _2)
        unless (i >= length is || i < 0) $ do
            modify' $ runOp $ is !! i
            modify' $ _2 %~ succ
            go
            
part2 :: [Instruction] -> Registers
part2 is = execState go (1,0,0,0,0,0)
    where
    go = do
        t <- get
        let i = t ^. _2 
        if i >= length is then return t else do
            modify $ runOp $ is !! i
            modify $ _2 %~ succ
            go
            

    
data OpCode = 
     AddR
   | AddI
   | MulR
   | MulI
   | BanR
   | BanI
   | BorR
   | BorI
   | SetR
   | SetI
   | GtIR
   | GtRI
   | GtRR
   | EqIR
   | EqRI
   | EqRR
   deriving (Show, Eq, Ord, Enum, Bounded)
   
readOpCode :: String -> OpCode
readOpCode "addr" = AddR
readOpCode "addi" = AddI
readOpCode "mulr" = MulR
readOpCode "muli" = MulI
readOpCode "banr" = BanR
readOpCode "bani" = BanI
readOpCode "borr" = BorR
readOpCode "bori" = BorI
readOpCode "setr" = SetR
readOpCode "seti" = SetI
readOpCode "gtir" = GtIR
readOpCode "gtri" = GtRI
readOpCode "gtrr" = GtRR
readOpCode "eqir" = EqIR
readOpCode "eqri" = EqRI
readOpCode "eqrr" = EqRR
   
readInstruction :: String -> Instruction
readInstruction s = let [op,a,b,c] = words s 
    in (readOpCode op, read a, read b, read c)
    
   
type Instruction = (OpCode,Int,Int,Int)
type Registers = (Int,Int,Int,Int,Int,Int)
   
runop :: OpCode -> Int -> Int -> Int -> Registers -> Registers
runop op a b c t = f $ case op of 
    AddR -> t ^. l a + t ^. l b
    AddI -> t ^. l a + b
    MulR -> t ^. l a * t ^. l b
    MulI -> t ^. l a * b
    BanR -> t ^. l a .&. t ^. l b
    BanI -> t ^. l a .&. b
    BorR -> t ^. l a .|. t ^. l b
    BorI -> t ^. l a .|. b
    SetR -> t ^. l a
    SetI -> a
    GtIR -> mkBit $        a >  t ^. l b
    GtRI -> mkBit $ t ^. l a >         b
    GtRR -> mkBit $ t ^. l a >  t ^. l b
    EqIR -> mkBit $        a == t ^. l b
    EqRI -> mkBit $ t ^. l a ==        b
    EqRR -> mkBit $ t ^. l a == t ^. l b
    where
    f n = t & l c .~ n
    l n = case n of 
        0 -> _1
        1 -> _2
        2 -> _3
        3 -> _4
        4 -> _5
        5 -> _6
        
    mkBit b = if b then 1 else 0
    
possibleops :: Int -> Int -> Int -> Registers -> Registers -> [OpCode]
possibleops a b c t t' = filter f [minBound..maxBound]
  where f op = runop op a b c t == t'