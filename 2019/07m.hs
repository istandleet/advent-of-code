{-# language BangPatterns #-}
module Main where

import Data.Foldable
import qualified Data.List

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "07.txt" :: IO Program
    print $ p1 dat
    print $ p2 dat
    
p1 :: Program -> Int
p1 vec = maximum $ map (runP1 vec) $ Data.List.permutations [0..4]
    
runP1 :: Program -> [Int] -> Int
runP1 v = foldl' go 0
    where
    -- go !o i = getOutput [i,o] v
    go !o i = case runToPause $ initComputer v of
        NeedsInput f -> case runToPause $ f i of
            NeedsInput f' -> case runToPause $ f' o of
                HasOutput (out,_) -> out
        
p2 :: Program -> Int
p2 vec = maximum $ map (runP2 vec) $ Data.List.permutations [5..9]

runP2 :: Program -> [Int] -> Int
runP2 v = go . alterHead . feedInputs . zip (replicate 5 $ runToPause $ initComputer v) . map pure
    where
    alterHead ((NeedsInput f,[]):ss) = (runToPause $ f 0,[]):ss
    
    rotate (a:ss) = ss++[a]
    feedOutput ((HasOutput(o,s),is):(b,is'):ss) = (runToPause s,is):(b,is'++[o]):ss
    feedOutput ss = ss
    
    feedInput (NeedsInput f) (i:is) = (runToPause (f i),is)
    feedInput pr ins = (pr,ins)
    
    feedOutputs = foldr (.) id $ replicate 5 (rotate.feedOutput)
    feedInputs = map $ uncurry feedInput
    
    go ss = if all (isStopped.fst) ss then head $ snd $ head ss else go $ feedInputs $ feedOutputs $ ss

-- * Utils
isStopped :: PauseReason -> Bool
isStopped Stopped = True
isStopped _ = False