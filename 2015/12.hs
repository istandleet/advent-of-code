{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Lens
import Data.Aeson as A
import qualified Data.List

main = do
    putStrLn "12"
    Just i <- A.decodeFileStrict' "12.json"
    print $ part1 i
    print $ part2 i
    
-- part1 :: A.Value -> Int
part1 (Number i) = i
part1 (Array a)  = sum $ fmap part1 a
part1 (Object a) = sum $ fmap part1 a
part1 _ = 0

part2 (Number i) = i
part2 (Array a)  = sum $ fmap part2 a
part2 (Object a) 
    | any (== String "red") a = 0
    | otherwise = sum $ fmap part2 a
part2 _ = 0