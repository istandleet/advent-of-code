{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts(the,groupWith)

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

time = 2503

main = do
    putStrLn "14"
    input <- getInput
    print $ maximum $ map (distanceTraveled time) input
    print $ Data.List.maximumBy (compare `on` snd) $ countof $ foldMap (\s -> map name $ winnersAfter s input) [1..time]

getInput :: IO [Line]
getInput = readFile "14.txt" >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack

winnersAfter :: Int -> [Line] -> [Line]
winnersAfter s ls = 
    let ds = map (distanceTraveled s) ls
        m = maximum ds
     in [l | (d,l) <- zip ds ls, d == m]

distanceTraveled :: Int -> Line -> Int
distanceTraveled s Line{..} = full*duration*speed + speed*(duration `min` partial)
    where
    (full,partial) = s `divMod` (duration+rest)

type Name = Text
data Line = Line
   { name     :: !Name
   , speed    :: {-# unpack #-} !Int
   , duration :: {-# unpack #-} !Int
   , rest     :: {-# unpack #-} !Int
   } deriving (Eq,Ord,Show)

parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n" 

parseLine :: Parser Line
parseLine = do
    -- Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds.
    name <- word
    " can fly "
    speed <- decimal
    " km/s for "
    duration <- decimal
    " seconds, but then must rest for "
    rest <- decimal
    " seconds."
    return Line{..}
    where
    word = T.pack <$> many1' letter
    
countof :: Ord a => [a] -> [(a,Int)]
countof ts = [(the t,length t)|t <- ts, then group by t using groupWith]