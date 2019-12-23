{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Attoparsec.Text hiding (take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "25"
    actions <- getInput
    go actions 1 [1..]
    where
    go actions l (i:is) = do
        -- when (i `mod` 50 == 0) $ print i 
        let l' = commonPrefix (part1' i actions) (take 100 $ cycle [0,1])
        -- print $ take 50 t100
        if l'>l
            then print (i,l') *> go actions l' is
            else go actions l is

commonPrefix :: Eq a => [a] -> [a] -> Int
commonPrefix = go 0
    where
    go !i [] _ = i
    go !i _ [] = i
    go !i (a:as) (b:bs) = if a /= b then i else go (succ i) as bs
    
getInput :: IO [Action]
getInput = readFile "25.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
      
part1 :: [Action] -> Int
part1 actions = head
    [ a_val
    | a_val <- [1..]
    , take 100 (part1' a_val actions) == take 100 (cycle [1,0])
    ]

part1' :: Int -> [Action] -> [Int]
part1' a_val actions = go (0,initial)
  where
  initial = Map.singleton 'a' a_val <> Map.fromList (map (,0) ['b'..'d'])
  is = [0..length actions -1]
  go (i,d) 
    | i `notElem` is = []
    | otherwise = 
        let (i',d',mo) = applyAction (actions !! i) (i,d)
         in case mo of 
              Just o -> o : go (i',d')
              Nothing -> go (i',d')
    
type Wire = Char
type Val = Either Wire Int
type Dat = Map Wire Int
      
data Action = 
    Cpy Val Wire
  | Inc Wire
  | Dec Wire
  | Out Wire
  | Jnz Val Val
  deriving (Show,Eq)

applyAction :: Action -> (Int,Dat) -> (Int,Dat,Maybe Int)
applyAction act (i,d) = case act of 
    Cpy v w -> (i+1,Map.insert w (valof v) d,Nothing)
    Inc w   -> (i+1,Map.adjust succ w d,Nothing)
    Dec w   -> (i+1,Map.adjust pred w d,Nothing)
    Jnz w v -> (if valof w /= 0 then i+valof v else i+1,d,Nothing)
    Out w   -> (i+1,d,Just $ d Map.! w)
    where
    valof = either (d Map.!) id

parseAction :: Parser Action
parseAction = choice [cpy, inc', dec, jnz, out] 
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
    out = do
        "out "
        a <- parseWire
        pure $ Out a
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