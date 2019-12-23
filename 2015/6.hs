{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Array.Unboxed

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "6"
    dims <- getInput
    print $ part1 dims
    print $ part2 dims
    
type Board = UArray (Int,Int) Bool

part1 :: [Line] -> Int
part1 lines = length $ filter id $ elems $ execState go initial
    where
    initial = listArray ((0,0),(999,999)) $ repeat False
    go = mapM (modify . useLine) lines
    

useLine :: Line -> Board -> Board
useLine (Line TurnOff begin end) b = b // [(i,False) | i <- range (begin,end)]
useLine (Line TurnOn  begin end) b = b // [(i,True ) | i <- range (begin,end)]
useLine (Line Toggle  begin end) b = accum (const . not) b [(i,undefined) | i <- range (begin,end)]

type Board' = UArray (Int,Int) Int

part2 :: [Line] -> Int
part2 lines = sum $ elems $ execState go initial
    where
    initial = listArray ((0,0),(999,999)) $ repeat 0
    go = mapM (modify . useLine') lines
    

useLine' :: Line -> Board' -> Board'
useLine' (Line TurnOff begin end) b = accum go b [(i,undefined) | i <- range (begin,end)]
  where go e _ = max 0 (e-1)
useLine' (Line TurnOn  begin end) b = accum go b [(i,undefined) | i <- range (begin,end)]
  where go e _ = e+1
useLine' (Line Toggle  begin end) b = accum go b [(i,undefined) | i <- range (begin,end)]
  where go e _ = e+2


getInput :: IO [Line]
getInput = readFile "6.txt"
      >>= either fail return . parseOnly parseLines . T.pack
      
data Action = TurnOff | TurnOn | Toggle deriving (Eq,Ord,Show)
data Line = Line
   { action :: !Action
   , begin  :: {-# unpack #-} !(Int,Int)
   , end    :: {-# unpack #-} !(Int,Int)
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = do
    action <- parseAction
    char ' '
    a <- decimal
    char ','
    b <- decimal
    let begin = (a,b)
    " through "
    a <- decimal
    char ','
    b <- decimal
    let end = (a,b)
    return Line{..}

parseAction :: Parser Action
parseAction = ( TurnOff <$ "turn off" )
          <|> ( TurnOn  <$ "turn on"  )
          <|> ( Toggle  <$ "toggle"   )
    
    
-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390