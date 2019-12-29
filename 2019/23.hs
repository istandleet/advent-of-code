{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "23.txt" :: IO Program
    print $ p1 dat
    
type Network = Vector ([Int], Computer)

-- p1 :: Program -> Int
p1 = take 10
   . map (fmap fst)
   . iterate step
   . V.imap (\i comp -> ([i],comp)) . V.replicate 50 . initComputer

step :: Network -> Network
step = execState $ mapM_ processIx [0..49]
        
        
sendMessages :: [Int] -> Network -> Network
sendMessages [] = id
sendMessages (255:x:y:is) = error $ "Send to 255: " ++ show (x,y)
sendMessages (i:x:y:is) = sendMessages is . modifyAt i (_1 %~ (<> [x,y]))

processIx :: Int -> State Network ()
processIx ix = do
    (is,comp) <- (V.! ix) <$> get
    let ins = if null is then [-1] else is 
        (os,comp') = runState (interactSt ins) comp 
    modify $ sendMessages os
    modify $ writeAt ix ([],comp') 
    
     
writeAt  ix v = V.modify $ \vec -> VM.write vec ix v
modifyAt ix f = V.modify $ \vec -> VM.read vec ix >>= \v -> VM.write vec ix (f v)
