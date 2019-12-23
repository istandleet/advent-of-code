{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Lens
import Data.Char
import qualified Data.List

main = do
    putStrLn "11"
    let p' = incpass s
    putStrLn $ pass p'
    putStrLn $ pass $ incpass p'
    
    
s = upass "cqjxjnds"

type Pass = (Int,Int,Int,Int,Int,Int,Int,Int)

incpass p = if isValid pass' then pass' else incpass pass'
    where
    pass' = go ls p
    
    -- go :: [Lens' Pass Int] -> Pass -> Pass
    go ((g,s):ls) p 
        | p ^. g /= 22 = p & s %~ succ
        | otherwise = go ls p & s .~ 0

    ls = zip getters setters
    setters = reverse [_1,_2,_3,_4,_5,_6,_7,_8]
    getters = reverse [_1,_2,_3,_4,_5,_6,_7,_8]
        
isValid :: Pass -> Bool
isValid p =
       hasIncreasing p
    && hasDoubleDouble p
       
hasIncreasing :: Pass -> Bool
hasIncreasing p = any go ls
    where
    go (a,(b,c)) = succ (chr $ p ^. a + 97) ==  chr (p ^. b + 97)
                && succ (chr $ p ^. b + 97) ==  chr (p ^. c + 97)
    ls = zip getters (zip (tail getters) (tail $ tail getters))
    getters = [_1,_2,_3,_4,_5,_6,_7,_8]
    
hasDoubleDouble :: Pass -> Bool
hasDoubleDouble p = go ls
    where
    go [] = False
    go [_] = False
    go ((a,b):ls') = if p ^. a == p ^. b 
        then any (\(a,b) -> p ^. a == p ^. b) (tail ls') 
        else go ls'
    
    ls = zip getters (tail getters)
    getters = [_1,_2,_3,_4,_5,_6,_7,_8]
        
pass :: Pass -> String
pass p = map char [p^._1,p^._2,p^._3,p^._4,p^._5,p^._6,p^._7,p^._8]

upass :: String -> Pass
upass p = (uchar $ p!!0,uchar $ p!!1,uchar $ p!!2,uchar $ p!!3,uchar $ p!!4,uchar $ p!!5,uchar $ p!!6,uchar $ p!!7)

char :: Int -> Char
char i
    | i < 8     = chr (97+i)
    | i < 10    = chr (97+i+1)
    | i < 12    = chr (97+i+2)
    | otherwise = chr (97+i+3)
uchar :: Char -> Int
uchar c
    | c < 'i'   = ord c - 97
    | c < 'l'   = ord c - 97 - 1
    | c < 'o'   = ord c - 97 - 2
    | otherwise = ord c - 97 - 3