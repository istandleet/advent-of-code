{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
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
    s <- readFile "21.txt"
    input <- either fail pure $ parseOnly parseLines $ T.pack s
    print $ part1 input -- 2724
    putStrLn $ part2 input

part1 :: [Line] -> Int
part1 input = sum $ map length answer
    where 
    ingredients = map fromLine input
    imps = impossible ingredients
    answer = map (Set.intersection imps . fst) ingredients

impossible :: [Ingredients] -> Set RawIngredient
impossible ingredients = foldMap fst ingredients `Set.difference` possible
    where
    possible = foldMap id (candidates ingredients)
    impossible = foldMap fst ingredients `Set.difference` possible

candidates :: [Ingredients] -> Map Allergen (Set RawIngredient)
candidates ls = answer
    where
    all_allergens = foldMap snd ls
    answer = Map.fromSet go all_allergens
    go allergen = foldl1 Set.intersection $ map fst $ filter (Set.member allergen . snd) ls

part2 :: [Line] -> String
part2 input = T.unpack . T.intercalate ","
            . map ((raws !!) . snd)
            . Data.List.sortOn ((allergens !!) . fst)
            $ solvedIndices solution
    where 
    ingredients = map fromLine input
    imps = impossible ingredients
    important = map (_1 %~ flip Set.difference (impossible ingredients)) ingredients
    allergens = Map.keys $ candidates important
    raws = Set.toList $ foldMap id $ candidates important
    solution = trivialSimplification $ toArray important

type Allergen = Text
type RawIngredient = Text
type Ingredients = (Set RawIngredient, Set Allergen)
fromLine :: Line -> Ingredients
fromLine (a, b) = (Set.fromList $ map T.pack a, Set.fromList $ map T.pack b)

toArray :: [Ingredients] -> Array (Int, Int) Bool
toArray ingredients = array bnds 
    [ ((aix,rix), go allergen raw)
    | (aix, allergen) <- zip [0..] allergens
    , (rix, raw) <- zip [0..] raws
    ]
    where
    allergens = Map.keys $ candidates ingredients
    raws = Set.toList $ foldMap id $ candidates ingredients
    go allergen raw = 
        let f (raws, allergens) = 
                if allergen `Set.member` allergens
                    then raw `Set.member` raws
                    else True
         in all f ingredients
    bnds = ((0, 0),(pred $ length allergens, pred $ length raws))

trivialSimplification :: Array (Int, Int) Bool -> Array (Int, Int) Bool
trivialSimplification = fst . head . dropWhile (uncurry (/=)) . (\a -> zip a (tail a)) . iterate trivialSimplification'

-- | for an ixj matrix does sudoku like simplification
trivialSimplification' :: Array (Int, Int) Bool -> Array (Int, Int) Bool
trivialSimplification' arr = array (bounds arr)
    [ (ix, arr!ix && not (ix `Set.member` known_wrong))
    | ix <- range $ bounds arr
    ]
    where 
    ((xmin,ymin),(xmax,ymax)) = bounds arr
    related (x,y) = map (,y) [xmin..xmax] ++ map (x,) [ymin..ymax]
    solved = Set.fromList $ solvedIndices arr
    known_wrong = foldMap (Set.fromList . related) solved Set.\\ solved

solvedIndices :: Array (Int, Int) Bool -> [(Int, Int)]
solvedIndices arr = filter solved $ range $ bounds arr
    where 
    ((xmin,ymin),(xmax,ymax)) = bounds arr
    solved ix@(x,y) = arr ! ix 
             && ( all (\ix' -> ix == ix' || not (arr!ix')) (map (,y) [xmin..xmax])
               || all (\ix' -> ix == ix' || not (arr!ix')) (map (x,) [ymin..ymax])
                )

-- * Parsing
type Line = ([String], [String])

parseLines :: P.Parser [Line]
parseLines = (parseLine `sepBy1'` endOfLine) <* skipSpace <* endOfInput

parseLine :: P.Parser Line
parseLine = do
    ingredients <- word `sepBy1'` char ' '
    " (contains "
    allergents <- word `sepBy1'` ", "
    char ')'
    return (ingredients, allergents)
    where 
    word = many1 P.letter
