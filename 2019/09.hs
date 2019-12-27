{-# language LambdaCase #-} 
module Main where

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "09.txt" :: IO Program
    print $ p1 dat
    print $ p2 dat
    
p1 :: Program -> [Int]
p1 = go . runToPause . initComputer
    where
    go = \case
        Stopped -> []
        HasOutput(o,s)->o:go (runToPause s)
        NeedsInput f -> go (runToPause $ f 1)
        
p2 :: Program -> [Int]
p2 = go . runToPause . initComputer
    where
    go = \case
        Stopped -> []
        HasOutput(o,s)->o:go (runToPause s)
        NeedsInput f -> go (runToPause $ f 2)

testInputs :: Program -> [Int]
testInputs = go . runToPause . initComputer
    where
    go = \case
        Stopped -> []
        HasOutput(o,s)->o:go (runToPause s)
