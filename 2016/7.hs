{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Ord
import Data.Maybe
import Data.Tuple
import qualified Data.List
import qualified Data.Set as Set
import GHC.Exts (groupWith,the)

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "7"
    s <- lines <$> readFile "7.txt"
    print $ length $ filter supportsTLS s
    print $ length $ filter supportsSSL s

supportsTLS :: String -> Bool
supportsTLS s = let (as,bs) = readLine s in any hasABBA as && not (any hasABBA bs)
supportsSSL :: String -> Bool
supportsSSL s = 
    let (as,bs) = readLine s 
        aabas = foldMap getABAs as
        babas = foldMap getABAs bs
     in any ((`elem` babas) . swap) aabas
    
readLine :: String -> ([String],[String])
readLine = go 
    where
    go s = case break (=='[') s of
        (a,[]) -> ([a],[])
        (a,b) -> case break (==']') b of
            (b',[]) -> ([s],[])
            (b',s') -> let (as,bs) = go (tail s') in (a:as,tail b':bs)


getABAs :: Eq a => [a] -> [(a,a)]
getABAs (a:b:c:s) = if a /= b && a == c then (a,b):getABAs (b:c:s) else getABAs (b:c:s)
getABAs _ = []
            
hasABBA :: Eq a => [a] -> Bool
hasABBA (a:b:c:d:s) = isABBA a b c d || hasABBA (b:c:d:s)
hasABBA _ = False

isABBA :: Eq a => a -> a -> a -> a -> Bool
isABBA a b c d = a /= b && a == d && b == c