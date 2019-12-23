{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Data.PF

import Control.Applicative
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Math.Combinat.Sets

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "20"
    print $ firstWith giftsGot  33100000
    print $ firstWith giftsGot' 33100000
    
    
firstWith f goal = head
    [ toInteger pf
    | pf <- primeFactors
    , f pf >= goal
    ]
    
giftsGot :: PF -> Integer
giftsGot pf =
    let factors = map toInteger $ pffactors pf
     in sum $ map (*10) factors
     
giftsGot' :: PF -> Integer
giftsGot' pf =
    let int = toInteger pf
        factors = map toInteger $ pffactors pf
        applicable = dropWhile (\n -> n * 50 < int) factors
     in sum $ map (*10) applicable
    
pffactors :: PF -> [PF]
pffactors (PF pf) = map (PF . filter ((>0) . snd)) $ listTensor [[(p,k) | k <- [0..pow]]| (p,pow) <- pf]