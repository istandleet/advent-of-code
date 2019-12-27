module Main where

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "05.txt" :: IO Program
    print $ prob 1 dat -- 11933517
    print $ prob 5 dat -- 10428568
    
prob :: Int -> Program -> Int
prob num = last . runOneInput num . initComputer

runOneInput :: Int -> Computer -> [Int]
runOneInput i c = case runToPause c of
    Stopped -> []
    NeedsInput f -> runOneInput i $ f i
    HasOutput (o,c) -> o : runOneInput i c
    