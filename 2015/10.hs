{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import qualified Data.List

main = do
    putStrLn "9"
    print $ length $ iterate lookAndSay s !! 40
    print $ length $ iterate lookAndSay s !! 50

lookAndSay :: String -> String
lookAndSay = foldMap go . Data.List.group
    where
    go x = show (length x) ++ [head x]
    
s = "3113322113"