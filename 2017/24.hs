{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
{-# language BangPatterns #-}
module Main where 

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Either
import Text.Read (readMaybe)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

main = do
    putStrLn "24"
    ls <- readInput <$> readFile "24.txt"
    print $ part1 ls
    print $ part2 ls
    
ex = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"


type Line = (Int,Int)
readInput :: String -> [Line]
readInput = map go . lines 
    where go s = let (a,b) = break (=='/') s in (read a, read $ tail b)

part1 :: [Line] -> Int
part1 = maximum . map (sum . map (uncurry (+))) . part1'

part1' :: [Line] -> [[Line]]
part1' = go 0 . Seq.fromList
    where
    go !x ls | not (any (\l -> fst l == x || snd l == x) ls) = [[]]
    go !x ls = do
        (i,l) <- zip [0..] (toList ls)
        guard $ fst l == x || snd l == x
        let ls' = Seq.deleteAt i ls
            y   = if fst l == x then snd l else fst l
        (l:) <$> go y ls'
        
part2 :: [Line] -> Int
part2 = maximum . map (sum . map (uncurry (+))) . go . part1'
    where
    go ls = let m = maximum $ map length ls in filter ((==m) . length) ls