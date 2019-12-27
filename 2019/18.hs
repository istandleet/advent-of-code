{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    s <- readFile "18.txt"
    print $ p1 $ parseBoard s
    
p1 :: Board -> Maybe Int
-- p1 b = Data.List.findIndex (any $ (== length (keyLocs b)) . length . _keys) $ rangeTo b
p1 b = Data.List.findIndex (any $ any $ (== length (keyLocs b)) . length) $ rangeTo' b
    
-- * Robot
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Board = Board
   { openSquares :: !(Set Coord)
   , keyLocs :: !(Map Coord Char)
   , doorLocs :: !(Map Coord Char)
   , doorMaps :: !(Map Char Coord)
   , entryPoint :: !Coord
   } deriving (Eq, Show)
   
data State = State
   { _pos  :: !Coord
   , _keys :: !(Set Char)
   } deriving (Eq, Ord, Show)
   
isNotBetterThan (State a b) (State a' b') = a == a' && b `Set.isSubsetOf` b'
   
type OptimalState = Map Coord [Set Char]

integrateState :: State -> OptimalState -> OptimalState
integrateState (State ix s) = Map.alter go ix
    where
    go mss = Just $ case mss of
        Nothing -> [s]
        Just ss -> if any (s `Set.isSubsetOf`) ss then ss else s : filter (not . (`Set.isSubsetOf` s)) ss
        
integrateState' :: State -> OptimalState -> Maybe OptimalState
-- integrateState' (State ix s) os = case Map.lookup ix os of 
--     Nothing -> Just $ Map.insert ix [s] os
--     Just ss -> if any (s `Set.isSubsetOf`) ss 
--         then Nothing 
--         else Just $ Map.insert ix (s : filter (not . (`Set.isSubsetOf` s)) ss) os
integrateState' (State ix s) = Map.alterF go ix
    where
    go mss = case mss of
        Nothing -> Just $ Just $ [s]
        Just ss -> if any (s `Set.isSubsetOf`) ss then Nothing else Just $ Just $ s : filter (not . (`Set.isSubsetOf` s)) ss

initialState :: Board -> State
initialState b = State (entryPoint b) mempty

move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ pred
    L -> _1 %~ succ
    R -> _1 %~ pred
    D -> _2 %~ succ
    
oppositeDir :: Dir -> Dir
oppositeDir = \case
    U -> D
    L -> R
    R -> L
    D -> U

rangeTo' :: Board -> [OptimalState]
rangeTo' board = go (Map.singleton (entryPoint board) [mempty]) [initialState board]
    where
    go !acc !new | null new = [acc]
    go !acc !new = acc : go acc' new'
        where 
        (acc',new') = foldl' (uncurry integrateWith) (acc,[]) 
                    $ foldMap (validMoves board) new 
        
    integrateWith !acc !new s = case integrateState' s acc of 
        Nothing -> (acc,new)
        Just acc' -> (acc',s:new)
        
rangeTo :: Board -> [Set State]
rangeTo board = go mempty (Set.singleton $ initialState board)
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' (new' Set.\\ acc')
        where 
        acc' = acc <> new
        new' = foldMap (validMoves board) new 
        
validMoves :: Board -> State -> Set State
validMoves board st = Set.map (\ix -> moveTo board ix st) openMoves
    where 
    openMoves = Set.intersection passableSquares $ Set.map (`move` _pos st) ds
    passableSquares = openSquares board 
               Set.\\ (Set.fromList $ Map.elems $ Map.withoutKeys (doorMaps board) (st ^. keys))
    ds = Set.fromList [minBound..maxBound]
    
moveTo :: Board -> Coord -> State -> State
moveTo board ix = case Map.lookup ix $ keyLocs board of
    Nothing -> pos .~ ix 
    Just c  -> (pos .~ ix) . (keys %~ Set.insert c)

-- * Parsing
parseBoard :: String -> Board
parseBoard s = Board
    { openSquares = Set.fromList . map fst . filter (isOpenChar . snd) . assocs $ arr
    , keyLocs = Map.fromList $ filter (isLower . snd) $ assocs arr
    , doorLocs = Map.fromList doorList
    , doorMaps = Map.fromList $ map swap doorList
    , entryPoint = fst $ head $ filter ((=='@') . snd) $ assocs arr
    }
    where
    arr = charArray s
    doorList = map (_2 %~ toLower) $ filter (isUpper . snd) $ assocs arr
    
isOpenChar :: Char -> Bool
isOpenChar c = isAlpha c || c == '.' || c == '@'
             
-- * Utils
blockChar :: Char
blockChar = '█'

charArray :: String -> UArray Coord Char
charArray s = array ((0,0),(x1,y1))
    [ ((x,y),c)
    | (y,l) <- zip [0..y1] $ lines s
    , (x,c) <- zip [0..x1] l
    ]
    where
    y1 = pred $ length $ filter (not . null) $ lines s
    x1 = pred $ length $ head $ lines s

neighbors :: IArray arr e => arr Coord e -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d)
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
-- * Ex
-- 8
ex = "#########\
\\n#b.A.@.a#\
\\n#########"

-- 86
ex1="########################\
\\n#f.D.E.e.C.b.A.@.a.B.c.#\
\\n######################.#\
\\n#d.....................#\
\\n########################"

-- 132
ex2="########################\
\\n#...............b.C.D.f#\
\\n#.######################\
\\n#.....@.a.B.c.d.A.e.F.g#\
\\n########################"

-- 136
ex3="#################\
\\n#i.G..c...e..H.p#\
\\n########.########\
\\n#j.A..b...f..D.o#\
\\n########@########\
\\n#k.E..a...g..B.n#\
\\n########.########\
\\n#l.F..d...h..C.m#\
\\n#################"

-- 81
ex4 = "########################\
\\n#@..............ac.GI.b#\
\\n###d#e#f################\
\\n###A#B#C################\
\\n###g#h#i################\
\\n########################"


-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
keys :: Lens' State (Set Char)
keys f_al9v (State x1_al9w x2_al9x)
  = (fmap (\ y1_al9y -> (State x1_al9w) y1_al9y)) (f_al9v x2_al9x)
{-# INLINE keys #-}
pos :: Lens' State Coord
pos f_al9z (State x1_al9A x2_al9B)
  = (fmap (\ y1_al9C -> (State y1_al9C) x2_al9B)) (f_al9z x1_al9A)
{-# INLINE pos #-}