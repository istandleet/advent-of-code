{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

main = do
    s <- readFile "1.txt"
    print $ part1 s
    print $ part2 s
    
part1 :: String -> Int
part1 = sum . map c
    where 
    c '(' = 1
    c _ = -1
    
    
part2 :: String -> Int
part2 = go 0 0
    where 
    c '(' = 1
    c _ = -1
    go i n (p:ps)
        | n < 0 = i
        | otherwise = go (i+1) (n+c p) ps