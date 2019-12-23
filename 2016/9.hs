{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Array.Unboxed
import qualified Data.List
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text as P hiding (D)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "9"
    s <- readFile "9.txt"
    print $ part1 s
    print $ part2 s
    

part1 :: String -> Either String Int
part1 = parseOnly part1p . T.pack

part1p :: Parser Int
part1p = do
    isend <- atEnd 
    if isend then return 0 else do
      e <- eitherP parseMarker anyChar
      case e of
        Right _ -> (+1) <$> part1p
        Left (w,n) -> P.take w >> (+(w*n)) <$> part1p

part1p' :: Parser Text
part1p' = fix $ \p -> do
    isend <- atEnd 
    if isend then return "" else do
      e <- eitherP parseMarker anyChar
      case e of
        Right c ->  T.cons c <$> p
        Left (w,n) -> do
            t <- P.take w 
            (T.replicate n t <>) <$> p
            
part2 :: String -> Either String Int
part2 = parseOnly part2p . T.pack

part2p :: Parser Int
part2p = fix $ \p -> do
    isend <- atEnd 
    if isend then return 0 else do
      e <- eitherP parseMarker anyChar
      case e of
        Right c ->  succ <$> p
        Left (w,n) -> do
            t <- P.take w 
            w' <- either fail return $ parseOnly p t
            (+(w'*n)) <$> p

part2p' :: Parser Text
part2p' = fix $ \p -> do
    isend <- atEnd 
    if isend then return "" else do
      e <- eitherP parseMarker anyChar
      case e of
        Right c ->  T.cons c <$> p
        Left (w,n) -> do
            t <- P.take w 
            t' <- either fail return $ parseOnly p t
            (T.replicate n t' <>) <$> p

parseMarker :: Parser (Int,Int)
parseMarker = do
    char '('
    a <- decimal
    char 'x'
    b <- decimal
    char ')'
    return (a,b)