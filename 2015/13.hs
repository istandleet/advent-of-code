{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "13"
    input <- getInput
    let ps = pairings input
    print $ circlingSalesman' ps
    let names = allNames' ps
        mes = foldMap (\n -> [(n,"Me",0),("Me",n,0)]) names
        ps' = mes ++ ps
    print $ circlingSalesman' ps'

getInput :: IO [Line]
getInput = readFile "13.txt" >>= either fail return . parseOnly parseLines . T.pack

pairings :: [Line] -> [(Name,Name,Int)]
pairings lines = 
    [ (p,p',a+b)
    | p <- names
    , p' <- names
    , p /= p'
    , let Just a = gain <$> Data.List.find (\l -> p1 l == p && p2 l == p') lines
    , let Just b = gain <$> Data.List.find (\l -> p2 l == p && p1 l == p') lines
    ]
    where
    names = Set.toList $ allNames lines
      
circlingSalesman' :: [Line'] -> Int
circlingSalesman' ls =
    let (c:cs) = Set.toList $ allNames' ls
        paths = map (c:) $ Data.List.permutations $ cs
     in maximum $ map (pathDistance ls) paths
     
allNames :: [Line] -> Set Name
allNames ss = Set.fromList (map p1 ss) <> Set.fromList (map p2 ss)

allNames' :: [Line'] -> Set Name
allNames' ss = Set.fromList (map ffst ss) <> Set.fromList (map ssnd ss)
    where
    ffst (a,b,c)=a
    ssnd (a,b,c)=b
      
findDistance :: Name -> Name -> [Line'] -> Int
findDistance a b ((a',b',i):ls)
    | (a == a' && b == b') || (b == a' && a == b') = i
    | otherwise = findDistance a b ls

pathDistance :: [Line'] -> [Name] -> Int
pathDistance ls cs = (sum $ map go $ zip cs $ tail cs) + go (head cs,last cs)
  where go (a,b) = findDistance a b ls
      
      
type Name = Text
type Line' = (Name,Name,Int)
data Line = Line
   { p1   :: !Name
   , gain :: {-# unpack #-} !Int
   , p2   :: !Name
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" <* skipSpace <* endOfInput

parseLine :: Parser Line
parseLine = do
    -- Alice would gain 54 happiness units by sitting next to Bob.
    p1 <- word
    " would "
    gainorloss <- ((-1) <$ "lose") <|> (1 <$ "gain")
    " "
    v <- decimal
    let gain = gainorloss * v
    " happiness units by sitting next to "
    p2 <- word
    "."
    return Line{..}
    where
    word = T.pack <$> many1' letter