module Main where 

import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.Set as Set

main = do
    s <- readFile "16.txt"
    let (p1s,s') = runState (repeatState $ state readPart1) s
        ls = filter (not . null) $ lines s'
        instructions = map readCode ls
    print $ part1 p1s
    print $ part2 p1s instructions

part1 :: [Part1] -> Int
part1 = length . filter ((>=3) . length . opcodesp1)
    
part2 :: [Part1] -> [(Int,Int,Int,Int)] -> Registers
part2 p1s instructions = 
    let opcodes = buildOpcodes p1s
        opcodes' = map (Set.findMin) opcodes
        instructions' :: [(OpCode, Int, Int, Int)]
        instructions' = map (\t -> t & _1 %~ (opcodes' !!)) instructions
        modder :: State Registers ()
        modder = mapM_ (\(o,a,b,c) -> modify $ runop o a b c) instructions'
     in execState modder (0,0,0,0)
    
buildOpcodes :: [Part1] -> [Set.Set OpCode]
buildOpcodes = applyN 16 shuffle_out_singles . foldr f initial
    where 
    initial = replicate 16 (Set.fromList [minBound..maxBound])
    f :: Part1 -> [Set.Set OpCode] -> [Set.Set OpCode]
    f (i,a,b,c,t,t') ss = 
        let ops = Set.fromList $ possibleops a b c t t'
         in ss & ix i %~ (Set.intersection ops)
    
    shuffle_out_singles ss = 
        let ones = filter ((==1) . length) ss
            filters = Set.unions ones
         in map (\s -> if length s == 1 then s else s Set.\\ filters) ss
        
applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n
    
readCode :: String -> (Int,Int,Int,Int)
readCode s = let [o,x,y,z] = map read $ words s in (o,x,y,z)

repeatState :: State s (Maybe a) -> State s [a]
repeatState s = do
    mx <- s
    case mx of 
        Just x -> (x:) <$> repeatState s
        Nothing -> pure []
    
type Part1 = (Int,Int,Int,Int,Registers,Registers)
readPart1 :: String -> (Maybe Part1,String)
readPart1 s = case lines s of
    (a:b:c:skip:ls) -> 
        let (beforestring,before) = splitAt 8 a
            (afterstring,after) = splitAt 7 c
         in case (skip,beforestring,afterstring) of
                ("","Before: ","After: ") -> 
                 let [b0,b1,b2,b3] = read before
                     [a0,a1,a2,a3] = read after
                     [o,x,y,z] = map read $ words b
                  in (Just (o,x,y,z,(b0,b1,b2,b3),(a0,a1,a2,a3)), unlines ls)
                _ -> (Nothing,s)
    _ -> (Nothing,s)
    
opcodesp1 :: Part1 -> [OpCode]
opcodesp1 (_,a,b,c,t,t') = possibleops a b c t t'
    
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
   
type Registers = (Int,Int,Int,Int)
   
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
        
    mkBit b = if b then 1 else 0
    
possibleops :: Int -> Int -> Int -> Registers -> Registers -> [OpCode]
possibleops a b c t t' = filter f [minBound..maxBound]
  where f op = runop op a b c t == t'