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
import qualified Data.Set as Set
import qualified Data.List
import Data.Array.Unboxed 

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Primes


main = do
    putStrLn "4"
    input <- lines <$> readFile "4.txt"
    print $ length $ filter isValid1 $ map T.pack input
    print $ length $ filter isValid2 input
    
    
isValid1 :: Text -> Bool
isValid1 t = let ws = T.words t in length (Set.fromList ws) == length ws
isValid2 :: String -> Bool
isValid2 t = let ws = map Data.List.sort $ words t in length (Set.fromList ws) == length ws