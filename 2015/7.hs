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
    putStrLn "7"
    input <- getInput
    let p1 = part1 input Map.! "a"
    print p1
    let p2 = part2 input p1 Map.! "a"
    print p2

getInput :: IO [Line]
getInput = readFile "7.txt"
      >>= either fail return . parseOnly parseLines . T.pack
      
      
part1 :: [Line] -> Dat
part1 = go mempty []
  where
  go dat [] [] = dat
  go dat passed [] = go dat [] passed
  go dat passed (l:ls) = case uncurry applyAction l dat of
    Nothing -> go dat (l:passed) ls
    Just dat' -> go dat' passed ls
    
part2 :: [Line] -> Word16 -> Dat
part2 lines a = go initial [] lines'
  where
  lines' = filter ((/= "b") . snd) lines
  initial = Map.singleton "b" a
  go dat [] [] = dat
  go dat passed [] = go dat [] passed
  go dat passed (l:ls) = case uncurry applyAction l dat of
    Nothing -> go dat (l:passed) ls
    Just dat' -> go dat' passed ls
      
type Wire = Text
type Val = Either Wire Word16
type Dat = Map Wire Word16
type Line = (Action,Wire)
      
data Action = 
    Store Val
  | And Val Val
  | Or Val Val
  | Lshift Wire Int
  | Rshift Wire Int
  | Not Val
  deriving (Show,Eq)

applyAction :: Action -> Wire -> Dat -> Maybe Dat
-- returns Nothing if that wire is already in the dat or if the required wires are not satisfied. 
applyAction act w d
    | w `Map.member` d = Nothing
    | not satisfied = Nothing
    | otherwise = Just $ Map.insert w val d
    where
    valsat = either (`Map.member` d) (const True)
    satisfied = case act of 
      Store v -> valsat v
      And a b -> valsat a && valsat b
      Or a b -> valsat a && valsat b
      Lshift a _ -> a `Map.member` d
      Rshift a _ -> a `Map.member` d
      Not a -> valsat a
      
    valof = either (d Map.!) id
    val = case act of 
      Store v -> valof v
      And a b -> valof a .&. valof b
      Or a b  -> valof a .|. valof b
      Lshift a n -> (d Map.! a) `shiftL` n
      Rshift a n -> (d Map.! a) `shiftR` n
      Not a -> complement $ valof a

parseAction :: Parser Action
parseAction = choice $ map (<* " -> ")
    [store , and' , or' , lshift , rshift , not' ] 
    where
    store = Store <$> parseVal
    and' = do
        a <- parseVal
        " AND "
        b <- parseVal
        pure $ And a b
    or' = do
        a <- parseVal
        " OR "
        b <- parseVal
        pure $ Or a b
    lshift = do
        a <- parseWire
        " LSHIFT "
        b <- decimal
        pure $ Lshift a b
    rshift = do
        a <- parseWire
        " RSHIFT "
        b <- decimal
        pure $ Rshift a b
    not' = do
        "NOT "
        a <- parseVal
        pure $ Not a 

parseWire :: Parser Wire
parseWire = T.pack <$> many1' letter <?> "wire"
parseVal :: Parser Val
parseVal = eitherP parseWire decimal

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" -- <* endOfInput

parseLine :: Parser Line
parseLine = do
    action <- parseAction
    wire <- parseWire
    return (action,wire)

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390

s :: Text
s = "123 -> x\n\
\456 -> y\n\
\x AND y -> d\n\
\x OR y -> e\n\
\x LSHIFT 2 -> f\n\
\y RSHIFT 2 -> g\n\
\NOT x -> h\n\
\NOT y -> i"