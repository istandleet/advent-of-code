{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Control.Applicative
import Data.Bits
import Data.Word

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main = do
    putStrLn "12"
    input <- getInput
    print $ part1 input Map.! 'a'
    print $ part2 input Map.! 'a'

getInput :: IO [Action]
getInput = readFile "12.txt"
      >>= either fail return . parseOnly parseLines . T.pack
      
      
part1 :: [Action] -> Dat
part1 actions = go (0,initial)
  where
  initial = Map.fromList $ map (,0) ['a'..'d']
  is = [0..length actions -1]
  go (i,d) 
    | i `notElem` is = d
    | otherwise = go $ applyAction (actions !! i) (i,d)
    
part2 :: [Action] -> Dat
part2 actions = go (0,initial)
  where
  initial = Map.fromList [('a',0),('b',0),('c',1),('d',0)]
  is = [0..length actions -1]
  go (i,d) 
    | i `notElem` is = d
    | otherwise = go $ applyAction (actions !! i) (i,d)
      
type Wire = Char
type Val = Either Wire Int
type Dat = Map Wire Int
      
data Action = 
    Cpy Val Wire
  | Inc Wire
  | Dec Wire
  | Jnz Val Val
  deriving (Show,Eq)

applyAction :: Action -> (Int,Dat) -> (Int,Dat)
applyAction act (i,d) = case act of 
    Cpy v w -> (i+1,Map.insert w (valof v) d)
    Inc w   -> (i+1,Map.adjust succ w d)
    Dec w   -> (i+1,Map.adjust pred w d)
    Jnz w v -> (if valof w /= 0 then i+valof v else i+1,d)
    where
    valof = either (d Map.!) id

parseAction :: Parser Action
parseAction = choice [cpy, inc', dec, jnz] 
    where
    cpy = do
        "cpy "
        a <- parseVal
        " "
        b <- parseWire
        pure $ Cpy a b
    inc' = do
        "inc "
        a <- parseWire
        pure $ Inc a
    dec = do
        "dec "
        a <- parseWire
        pure $ Dec a
    jnz = do
        "jnz "
        a <- parseVal
        " "
        b <- parseVal
        pure $ Jnz a b

parseWire :: Parser Wire
parseWire = letter
parseVal :: Parser Val
parseVal = eitherP parseWire (signed decimal)

parseLines :: Parser [Action]
parseLines = sepBy1' parseAction "\n" -- <* skipSpace <* endOfInput

s :: Text
s = "cpy 41 a\n\
\inc a\n\
\inc a\n\
\dec a\n\
\jnz a 2\n\
\dec a"