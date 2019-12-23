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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List
import Prelude hiding (EQ,GT,LT)

main = do
    putStrLn "8"
    input <- getInputs
    print $ part1 input
    print $ part2 input
    
part1 :: [Line] -> Int
part1 ls = maximum $ foldl go initial ls
    where
    initial = Map.fromSet (const 0) allnames
    allnames = foldMap (\(a,_,b,_,_) -> Set.fromList [a,b]) ls
    go acc (a,b,c,d,e) = if comparing d (acc Map.! c) e then Map.adjust (+b) a acc else acc
    
part2 :: [Line] -> Int
part2 ls = maximum $ map maximum $ scanl go initial ls
    where
    initial = Map.fromSet (const 0) allnames
    allnames = foldMap (\(a,_,b,_,_) -> Set.fromList [a,b]) ls
    go acc (a,b,c,d,e) = if comparing d (acc Map.! c) e then Map.adjust (+b) a acc else acc
    
comparing :: Comp -> Int -> Int -> Bool
comparing EQ  = (==)
comparing NE  = (/=)
comparing LT  = (<)
comparing LTE = (<=)
comparing GT  = (>)
comparing GTE = (>=)
    
getInputs :: IO [Line]
getInputs = map (readLine . words) . lines <$> readFile "8.txt"

type Line = (String, Int, String, Comp, Int)
data Comp = EQ | NE | LT | LTE | GT | GTE deriving (Show, Eq)

readComp :: String -> Comp
readComp "==" = EQ
readComp "!=" = NE
readComp "<"  = LT
readComp "<=" = LTE
readComp ">"  = GT
readComp ">=" = GTE

readLine :: [String] -> Line
readLine [a,b,c,"if",d,e,f] = (a,chng,d, readComp e, read f)
    where
    chng = read c * if b == "inc" then 1 else -1