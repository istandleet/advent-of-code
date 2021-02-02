{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
module Main where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

main :: IO ()
main = do
    s <- readFile "07.txt"
    input <- either fail pure $ parseOnly parseLines $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: [Line] -> Int
part1 = pred . length . flip findAncestors "shiny gold" . toAncestorBoard

findAncestors :: AncestorBoard -> Bag -> Set Bag
findAncestors board = go mempty . Set.singleton
    where
    go known new | null new = known
    go known new = 
        let known' = known <> new
            parents = foldMap directparents new
            new' = parents Set.\\ known'
         in go known' new'
    directparents b = case Map.lookup b board of
        Nothing -> mempty
        Just parents -> Set.fromList $ map fst $ board Map.! b

type AncestorBoard = Map Bag [(Bag,Int)] -- from bags to which bags can hold it (and how many they can hold)
toAncestorBoard :: [Line] -> AncestorBoard
toAncestorBoard ls = Map.fromList
    [ (the content, zip bag n)
    | (bag, contents) <- ls
    , (n, content) <- contents
    , then group by content using groupWith
    ]

part2 :: [Line] -> Int
part2 = flip findDescendents "shiny gold" . Map.fromList

type DescendentBoard = Map Bag [(Int,Bag)]
findDescendents :: DescendentBoard -> Bag -> Int
findDescendents board root = (Map.! root) $ go initial $ Set.singleton root
    where
    initial = 0 <$ Map.filter null board
    go known queue | null queue = known
    go known queue = 
        let children = Map.restrictKeys board queue :: DescendentBoard
            (newkids, newknown) = Map.mapEither (directchildren known) children
            known' = newknown <> known
            queue' = (queue <> fold newkids) Set.\\ Map.keysSet known'
         in go known' queue'
    directchildren :: Map Bag Int -> [(Int,Bag)] -> Either (Set Bag) Int
    directchildren known contents = 
        let subbags = Set.fromList $ map snd contents
            kc = Map.restrictKeys known subbags
         in if length kc /= length contents
              then Left subbags
              else Right $ sum $ map (\(n,b) -> n * (kc Map.! b) + n) contents

-- Parsing
type Bag = Text
type Line = (Bag, [(Int,Bag)])
word = many' P.letter

parseLines :: P.Parser [Line]
parseLines = (parseLine `sepBy1'` endOfLine) <* skipSpace <* endOfInput

-- pale turquoise bags contain 3 muted cyan bags, 5 striped teal bags.
-- dotted violet bags contain no other bags.
parseLine :: P.Parser Line
parseLine = do
    color <- parseColor
    " bags contain "
    contains <- (parseContains `sepBy1'` ", ") <|> (mempty <$ "no other bags")
    char '.'
    return (color, contains)

parseContains :: P.Parser (Int,Bag)
parseContains = do
    n <- decimal
    " "
    color <- parseColor
    if n == 1 then " bag" else " bags"
    return (n,color)

parseColor :: P.Parser Bag
parseColor = T.pack <$> do
    a <- word
    char ' '
    b <- word
    pure $ unwords [a,b]