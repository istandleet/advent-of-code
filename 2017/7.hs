{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Foldable
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Exts (the, groupWith)
import Data.Matrix hiding ((<|>))

import Data.Char
import Data.Attoparsec.Text hiding (D, count)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "7"
    input <- getInput
    let parent = Set.findMin $ part1 input
    print parent
    print $ part2 parent $ buildTree input
    
    
getInput :: IO [Line]
getInput = readFile "7.txt"
      >>= either fail return . parseOnly (parseLines <* skipSpace <* endOfInput) . T.pack
      
part1 :: [Line] -> Set Text
part1 = go . buildTree
    where
    go m = let allnodes = Map.keysSet m
               allcs = foldMap snd m
            in allnodes Set.\\ allcs
            
part2 :: Text -> Map Text (Int,Set Text) -> Int
part2 up m = 
    let imb = findImbalance up m
        Just p = parentOf imb m
        cs = Set.toList $ snd $ m Map.! p
        ws = map (`towerWeight` m) cs
        cws = zip cs ws
        Just imbtw = snd <$> Data.List.find ((==imb) . fst) cws
        Just nontw = snd <$> Data.List.find ((/=imb) . fst) cws
        imbw = fst $ m Map.! imb
     in (nontw-imbtw)+imbw

            
findImbalance :: Text -> Map Text (Int,Set Text) -> Text
findImbalance k m 
    = let (w,cs) = m Map.! k 
       in case findDifferent (`towerWeight` m) $ Set.toList cs of
            Nothing -> k
            Just k' -> findImbalance k' m 
    
parentOf :: Text -> Map Text (Int,Set Text) -> Maybe Text
parentOf k m = fmap fst $ Map.lookupMin $ Map.filter (\(_,cs) -> k `Set.member` cs) m

findDifferent :: Ord b => (a->b) -> [a] -> Maybe a
findDifferent f as = 
    let bs = map f as
        abs = zip as bs
        diffbs = Set.fromList bs
     in if length diffbs == 1 then Nothing else Just $ let (xs,ys) = Data.List.partition ((== head bs) . snd) abs in case (xs,ys) of
            ([x],_) -> fst x
            (_,[y]) -> fst y

towerWeight :: Text -> Map Text (Int,Set Text) -> Int
towerWeight k m = let (w,cs) = m Map.! k in if null cs then w else w + sum [towerWeight c m | c <- Set.toList cs] 
      
buildTree :: [Line] -> Map Text (Int,Set Text)
buildTree = Map.fromList . map (\Line{..} -> (name,(weight,Set.fromList children)))
      
data Line = Line
   { name     :: Text
   , weight   :: Int
   , children :: [Text]
   } deriving (Show, Eq)
parseLines :: Parser [Line]
parseLines = sepBy1' parseLine "\n"

-- fwft (72) -> ktlj, cntj, xhth
-- ktlj (57)
parseLine :: Parser Line
parseLine = do
    name <- word
    " ("
    weight <- decimal
    ")"
    children <- fmap (fromMaybe []) $ optional $ " -> " *> sepBy1' word ", "
    return Line{..}
    where
    word =  T.pack <$> many1' letter