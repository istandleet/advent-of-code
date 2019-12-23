{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.List
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Char
import Data.Attoparsec.Text hiding (D,take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "10"
    input <- getInput
    let (st,botins) = startSt input
        stages = iterate (runStage botins) st
    print $ part1 61 17 stages
    print $ part2 stages

part1 :: Int -> Int -> [St] -> Int
part1 a b ss = head
    [ fst $ IntMap.findMin im
    | st <- ss
    , let im = IntMap.filter isab $ fst st
    , not $ null im
    ]
    where
    isab (Two a' b') = (a == a' && b == b') || (a == b' && b == a')
    isab _ = False
    
part2 :: [St] -> IntMap [Int]
part2 ss = snd $ head
    [ a
    | (a,b) <- zip ss (tail ss)
    , a == b
    ]

runStage :: IntMap (Place,Place) -> St -> St
runStage botins st = 
    let bs = IntMap.mapMaybe toBoth $ fst st
        mods = IntMap.intersectionWithKey toMods botins bs
     in foldr ($) st mods

toMods :: Int -> (Place,Place) -> (Int,Int) -> St -> St
toMods giver (lo,hi) ab = (_1 %~ IntMap.insert giver None) . give lo (uncurry min ab) . give hi (uncurry max ab)

startSt :: [Line] -> (St,IntMap (Place,Place))
startSt = go . foldr breakup mempty
    where
    breakup (Value i p) (vs,bins) = ((i,p):vs,bins)
    breakup (BotIns i p1 p2) (vs,bins) = (vs,(i,(p1,p2)):bins)
    
    go (vs,bins) = (foldr (uncurry $ flip give) (allPlaces (vs,bins)) vs,IntMap.fromList bins)
    allPlaces (vs,bins) = 
        ( IntMap.fromList $ map (,None) $ 
            [i
            |Bot i <- map snd vs ++ map (fst . snd) bins ++ map (snd . snd) bins
            ]
        , IntMap.fromList $ map (,[]) $ 
            [i
            |Output i <- map snd vs ++ map (fst . snd) bins ++ map (snd . snd) bins
            ]
        )
      
give :: Place -> Int -> St -> St
give (Bot i) v = _1 %~ IntMap.adjust (add v) i
give (Output i) v = _2 %~ IntMap.adjust (v:) i

type St = (IntMap (UpTo2 Int),IntMap [Int])

data UpTo2 a = None | One a | Two a a deriving (Eq,Show)
add :: a -> UpTo2 a -> UpTo2 a
add v  None = One v
add v' (One v) = Two v v'
toBoth :: UpTo2 a -> Maybe (a,a)
toBoth (Two a b) = Just (a,b)
toBoth _ = Nothing


data Place = Bot Int | Output Int deriving (Eq,Show)
data Line = 
     Value Int Place
   | BotIns Int Place Place
   deriving (Eq,Show)

getInput :: IO [Line]
getInput = readFile "10.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

parseLine :: Parser Line
parseLine = parseVal <|> parseBotIns

parseVal :: Parser Line
parseVal = do
    "value "
    i <- decimal
    " goes to "
    p <- parsePlace
    return $ Value i p 
    
parseBotIns :: Parser Line
parseBotIns = do
    "bot "
    i <- decimal
    " gives low to "
    p1 <- parsePlace
    " and high to "
    p2 <- parsePlace
    return $ BotIns i p1 p2

parsePlace :: Parser Place
parsePlace = po <|> pb
    where
    po = fmap Output $ "output " *> decimal
    pb = fmap Bot $ "bot " *> decimal
    
exS = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
ex = either error id $ parseOnly parseLines exS