{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List

main = do
    putStrLn "2"
    input <- map (map (read . T.unpack) . T.split (=='\t') . T.pack) . lines <$> readFile "2.txt"
    print $ part1 input
    print $ part2 input
    
part1 :: [[Int]] -> Int
part1 = sum . map go
    where go ls = maximum ls - minimum ls
    
part2 :: [[Int]] -> Int
part2 = sum . map go
    where 
    go ls = sum 
        [ d
        | (x:xs) <- Data.List.tails ls
        , y <- xs
        , d <- maybeToList $ (x <||> y) <|> (y <||> x)
        ]
    
    
(<||>) :: Int -> Int -> Maybe Int
a <||> b = let (d,r) = a `divMod` b in d <$ guard (r==0)