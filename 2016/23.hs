{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}
module Main where 

import Control.Applicative
import Data.Bits
import Data.Word
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main = do
    putStrLn "23"
    input <- Seq.fromList <$> getInput
    print $ part1 input Map.! 'a'
    print $ part2 input Map.! 'a'

getInput :: IO [Action]
getInput = readFile "23.txt"
      >>= either fail return . parseOnly parseLines . T.pack
      
      
part1 :: Seq Action -> Dat
part1 actions = go (actions,0,initial)
  where
  initial = Map.fromList [('a',7),('b',0),('c',0),('d',0)]
  is = [0..length actions -1]
  go (as,i,d) 
    | i < 0 || i >= length actions = d
    | otherwise = go $ applyAction (as,i,d)
    
part2 :: Seq Action -> Dat
part2 actions = go (actions,0,initial)
  where
  initial = Map.fromList [('a',12),('b',0),('c',0),('d',0)]
  is = [0..length actions -1]
  go (as,i,d) 
    | i < 0 || i >= length actions = d
    | not $ any istoggle as = error $ show (as,i,d)
    | otherwise = go $ applyAction (as,i,d)
      
type Wire = Char
type Val = Either Wire Int
type Dat = Map Wire Int
      
data Action = 
    Cpy Val Val
  | Inc Wire
  | Dec Wire
  | Tgl Wire
  | Jnz Val Val
  deriving (Show,Eq)

toggle :: Action -> Action
toggle = \case
  Cpy a b -> Jnz a b
  Jnz a b -> Cpy a b
  Inc a -> Dec a
  Dec a -> Inc a
  Tgl a -> Inc a
istoggle :: Action -> Bool
istoggle = \case
  Tgl a -> True
  _ -> False
  
applyAction :: (Seq Action,Int,Dat) -> (Seq Action,Int,Dat)
applyAction (as,i,d) = case as `Seq.index` i of 
    Cpy v z -> (as,i+1,either (\w -> Map.insert w (valof v) d) (const d) z)
    Inc w   -> (as,i+1,Map.adjust succ w d)
    Dec w   -> (as,i+1,Map.adjust pred w d)
    Tgl w   -> (Seq.adjust' toggle (i + d Map.! w) as,i+1,d)
    Jnz w v -> (as,if valof w /= 0 then i+valof v else i+1,d)
    where
    valof = either (d Map.!) id

parseAction :: Parser Action
parseAction = choice [cpy, inc', dec, jnz, tgl] 
    where
    cpy = do
        "cpy "
        a <- parseVal
        " "
        b <- parseWire
        pure $ Cpy a (Left b)
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
    tgl = do
        "tgl "
        a <- parseWire
        pure $ Tgl a

parseWire :: Parser Wire
parseWire = letter
parseVal :: Parser Val
parseVal = eitherP parseWire (signed decimal)

parseLines :: Parser [Action]
parseLines = sepBy1' parseAction "\n" -- <* skipSpace <* endOfInput

s :: Text
s = "cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"

{-
cpy a b           12 12  0 0   
dec b             12 11  0 0   
cpy a d           12 11  0 12  -- a=a*b    
cpy 0 a            0 11  0 12  -- b--      
cpy b c            0 11 11 12  -- c=b*2    
inc a              1 11 11 12  -- d=0      
dec c              1 11 10 12  --          
jnz c -2          11 11  0 12  --          
dec d             11 11  0 11  --          
jnz d -5         132 11  0 0   --          
dec b            132 10  0 0   --          
cpy b c          132 10 10 0   --          
cpy c d          132 10 10 10  --          
dec d            132 10 10 9   --          
inc c            132 10 11 9   --          
jnz d -2         132 10 20 0   --          
tgl c            132 10 20 0   (...) inc c      479001600   0   0   0
cpy -16 c        132 10 -16 0                   479001600   0 -16   0
jnz 1 c                              cpy 1 c    479001600   0   1   0
cpy 85 c                                        479001600   0  85   0
jnz 91 d                             cpy 91 d   479001600   0  85  91
inc a                                           479001601   0  85  91
inc d                                dec d      479001601   0  85  90
jnz d -2                                        479001691   0  85   0
inc c                                dec c      479001691   0  84   0
jnz c -5                                        479009335   0   0   0
-}