{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)

main :: IO ()
main = do
    input <- readFile "11.txt" >>= either fail return . parseOnly parseInput . T.pack
    print $ part1 input
    print $ part2 input

part1 :: [Config] -> Int
part1 cs = monkeyBusiness $ go 20 mempty init
    where
    init = Map.fromList $ map (\c -> (num c, startingItems c)) cs
    go 0 m _ = m
    go n m b = let (m', b') = step cs b
                in go (pred n) (Map.unionWith (+) m m') b'

part2 :: [Config] -> Int
part2 cs = monkeyBusiness $ go 10000 mempty init
    where
    init = Map.fromList $ map (\c -> (num c, startingItems c)) cs
    mb = modBase cs
    go 0 m b = m
    go n m b = let (m', b') = step' mb cs b
                in go (pred n) (Map.unionWith (+) m m') b'

monkeyBusiness :: Map Int Int -> Int
monkeyBusiness = product . take 2 . reverse . Data.List.sort . Map.elems

data Config = Config 
   { num :: Int 
   , startingItems :: [Worry]
   , operation :: Operation
   , divTest :: Worry
   , trueMonkey :: Int
   , falseMonkey :: Int
   } deriving (Show, Eq, Ord)
type Worry = Integer

data Operation = Plus (Maybe Worry) | Times (Maybe Worry) deriving (Show, Eq, Ord)

performOp :: Operation -> Worry -> Worry
performOp = \case
    Plus (Just n) -> (+n)
    Plus Nothing -> \x -> x + x
    Times (Just n) -> (*n)
    Times Nothing -> \x -> x * x

type Board = Map Int [Worry]

modBase :: [Config] -> Worry
modBase = product . Set.fromList . map divTest

step :: [Config] -> Board -> (Map Int Int, Board)
step = go mempty
    where
    go m [] b = (m, b) 
    go m (c:cs) b = 
        let b' = Map.insert (num c) [] . Map.unionWith (<>) b $ stepMonkey c (b Map.! num c)
            m' = Map.insertWith (+) (num c) (length $ b Map.! num c) m
         in go m' cs b'

stepMonkey :: Config -> [Worry] -> Board
stepMonkey Config{..} items = Map.fromListWith (<>) $ do
    item <- items
    let item' = performOp operation item `div` 3
    if item' `mod` divTest == 0
        then [(trueMonkey, [item'])]
        else [(falseMonkey, [item'])]


step' :: Worry -> [Config] -> Board -> (Map Int Int, Board)
step' mb = go mempty
    where
    go m [] b = (m, b) 
    go m (c:cs) b = 
        let b' = Map.insert (num c) [] . Map.unionWith (<>) b $ stepMonkey' mb c (b Map.! num c)
            m' = Map.insertWith (+) (num c) (length $ b Map.! num c) m
         in go m' cs b'

stepMonkey' :: Worry -> Config -> [Worry] -> Board
stepMonkey' mb Config{..} items = Map.fromListWith (<>) $ do
    item <- items
    let item' = performOp operation item `mod` mb
    if item' `mod` divTest == 0
        then [(trueMonkey, [item'])]
        else [(falseMonkey, [item'])]


-- * Parsing
parseInput :: Parser [Config] 
parseInput = sepBy1' parseConfig skipSpace <* skipSpace <* endOfInput

parseConfig :: Parser Config
parseConfig = do
    "Monkey "
    num <- decimal
    ":"
    skipSpace
    "Starting items: "
    startingItems <- sepBy1' decimal (skipSpace *> char ',' *> skipSpace)
    skipSpace
    "Operation: new = old "
    operation <- parseOperation
    skipSpace
    "Test: divisible by "
    divTest <- decimal
    skipSpace
    "If true: throw to monkey "
    trueMonkey <- decimal
    skipSpace
    "If false: throw to monkey "
    falseMonkey <- decimal
    return Config{..}

parseOperation :: Parser Operation
parseOperation = do
    op <- char '+' <|> char '*'
    skipSpace
    n <- (Just <$> decimal) <|> ("old" $> Nothing) 
    return $ case op of
        '+' -> Plus n
        '*' -> Times n
        _ -> error "impossible"