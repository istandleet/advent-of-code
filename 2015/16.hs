{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Function
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts(the,groupWith)
import Math.Combinat.Partitions

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "15"
    input <- getInput
    print $ Data.List.find fits1 input
    print $ Data.List.find fits2 input
    

getInput :: IO [Line]
getInput = readFile "16.txt" >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack

fits1 :: Line -> Bool
fits1 a = all eq 
    [ children   
    , cats       
    , samoyeds   
    , pomeranians
    , akitas     
    , vizslas    
    , goldfish   
    , trees      
    , cars       
    , perfumes   
    ]
    where
    eq field = case field a of
        Nothing -> True
        Just  n -> field key == Just n
        
fits2 :: Line -> Bool
fits2 a = all eq
    [ children   
    , samoyeds   
    , akitas     
    , vizslas    
    , cars       
    , perfumes   
    ] && all gt
    [ cats       
    , trees      
    ] && all lt
    [ pomeranians
    , goldfish   
    ]
    where
    eq field = case field a of
        Nothing -> True
        Just  n -> field key == Just n
    lt field = case field a of
        Nothing -> True
        Just  n -> field key > Just n
    gt field = case field a of
        Nothing -> True
        Just  n -> field key < Just n

key = Line
    { lineNumber  = 0
    , children    = Just 3
    , cats        = Just 7
    , samoyeds    = Just 2
    , pomeranians = Just 3
    , akitas      = Just 0
    , vizslas     = Just 0
    , goldfish    = Just 5
    , trees       = Just 3
    , cars        = Just 2
    , perfumes    = Just 1
    }

type Name = Text
data Line = Line
   { lineNumber :: !Int
   , children   :: !(Maybe Int)
   , cats       :: !(Maybe Int)
   , samoyeds   :: !(Maybe Int)
   , pomeranians:: !(Maybe Int)
   , akitas     :: !(Maybe Int)
   , vizslas    :: !(Maybe Int)
   , goldfish   :: !(Maybe Int)
   , trees      :: !(Maybe Int)
   , cars       :: !(Maybe Int)
   , perfumes   :: !(Maybe Int)
   } deriving (Eq,Ord,Show)
   
instance Semigroup Line where
    a <> b = Line
        { lineNumber  = lineNumber  a
        , children    = children    a <|> children    b
        , cats        = cats        a <|> cats        b
        , samoyeds    = samoyeds    a <|> samoyeds    b
        , pomeranians = pomeranians a <|> pomeranians b
        , akitas      = akitas      a <|> akitas      b
        , vizslas     = vizslas     a <|> vizslas     b
        , goldfish    = goldfish    a <|> goldfish    b
        , trees       = trees       a <|> trees       b
        , cars        = cars        a <|> cars        b
        , perfumes    = perfumes    a <|> perfumes    b
        }

baseLine n = Line
    { lineNumber  = n
    , children    = Nothing
    , cats        = Nothing
    , samoyeds    = Nothing
    , pomeranians = Nothing
    , akitas      = Nothing
    , vizslas     = Nothing
    , goldfish    = Nothing
    , trees       = Nothing
    , cars        = Nothing
    , perfumes    = Nothing
    }

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" 

parseLine :: Parser Line
parseLine = do
    -- Sue 1: goldfish: 9, cars: 0, samoyeds: 9
    "Sue "
    lineNumber <- decimal
    ": "
    attrs <- sepBy1' (parserFor lineNumber) ", "
    return $ foldr (<>) (baseLine lineNumber) attrs
    where
    parserFor lineNumber = do
        children    <- optional $ "children: "    *> decimal
        cats        <- optional $ "cats: "        *> decimal
        samoyeds    <- optional $ "samoyeds: "    *> decimal
        pomeranians <- optional $ "pomeranians: " *> decimal
        akitas      <- optional $ "akitas: "      *> decimal
        vizslas     <- optional $ "vizslas: "     *> decimal
        goldfish    <- optional $ "goldfish: "    *> decimal
        trees       <- optional $ "trees: "       *> decimal
        cars        <- optional $ "cars: "        *> decimal
        perfumes    <- optional $ "perfumes: "    *> decimal
        return Line{..}