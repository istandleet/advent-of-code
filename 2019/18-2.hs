{-# language BangPatterns #-}
{-# language TupleSections #-} 
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
module Main (main) where

import Control.Applicative
import Control.Lens hiding (ix)
import Data.Char
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
    print $ p2 $ parseBoard ex0 -- 8
    print $ p2 $ parseBoard ex1 -- 24
    print $ p2 $ parseBoard ex2 -- 32
    print $ p2 $ parseBoard ex3 -- 72
    s <- readFile "18-2.txt" 
    print $ p2 $ parseBoard s
    
    

p2 :: Board -> Int
p2 board = go 0 (V.singleton first)
    where 
    first = Set.singleton $ initialState board
    totalKeys = length $ keyLocs board
    -- go :: [[State]] -> [[State]]
    go :: Int -> Vector (Set State) -> Int
    go !acc prog = case V.span null prog of 
        (ns,rest) | null ns -> 
            let !curr = V.head rest
                nxts = V.tail rest
             in case Data.List.find ((==totalKeys) . size . _keys) curr of
                    Just _ -> acc
                    Nothing -> go (succ acc) (foldr addStates nxts curr)
        (ns,nxts) -> go (acc+length ns) nxts
    addStates :: State -> Vector (Set State) -> Vector (Set State)
    addStates = foldr (.) id . map (uncurry insertAt) . possibleTargets board
    
insertAt :: Int -> State -> Vector (Set State) -> Vector (Set State)
insertAt i s initialV = V.force newV
    where 
    l = V.length initialV
    newV = case compare i l of
        LT -> let (a,b) = V.splitAt i initialV in a V.++ V.imap imapper b
        EQ -> initialV `V.snoc` Set.singleton s
        GT -> initialV V.++ (V.replicate (i-l) mempty `V.snoc` Set.singleton s)
    
    imapper 0 = Set.insert s . Set.filter (not . isBetterThan s)
    imapper _ = Set.filter (not . isBetterThan s)
        
-- * Robot
type Coord = (Int,Int)
type Positions = (Coord,Coord,Coord,Coord)
data Board = Board
   { openSquares :: !(Set Coord)
   , keyLocs :: !(Map Coord Char)
   , keyMaps :: !(Map Char Coord)
   , doorLocs :: !(Map Coord Char)
   , doorMaps :: !(Map Char Coord) 
   , entryPoint :: {-# unpack #-} !Positions
   } deriving (Eq, Show)
   
notMember :: Char -> KeySet -> Bool
withoutKeys :: Map Char a -> KeySet -> Map Char a
insert :: Char -> KeySet -> KeySet
size :: KeySet -> Int
isSubsetOf :: KeySet -> KeySet -> Bool

type KeySet = Set Char
notMember = Set.notMember
withoutKeys = Map.withoutKeys
insert = Set.insert
size = length
isSubsetOf = Set.isSubsetOf
   
data State = State
   { _pos  :: {-# unpack #-} !Positions
   -- , _keys :: {-# unpack #-} !KeySet
   , _keys :: !KeySet
   } deriving (Eq, Ord, Show)
   
isBetterThan :: State -> State -> Bool
State a b `isBetterThan` State a' b' = a == a' && b' `isSubsetOf` b

initialState :: Board -> State
initialState b = State (entryPoint b) mempty
    
possibleTargets :: Board -> State -> [(Int,State)]
possibleTargets board st = 
    [ (l-1,st & pos .~ p' & keys %~ insert keychar)
    | (keychar,keypos) <- Map.toList keysRemaining
    , (l,p') <- maybeToList $ findAndUpdate (fmap (,keypos) . distanceTo passableSquares keypos) (st ^. pos)
    ]
    where 
    keysRemaining   = withoutKeys (keyMaps board) (st ^. keys)
    passableSquares = openSquares board 
               Set.\\ (Set.fromList $ Map.elems $ withoutKeys (doorMaps board) (st ^. keys))
    
findAndUpdate :: forall a. (Coord -> Maybe (a,Coord)) -> Positions -> Maybe (a,Positions)
findAndUpdate f positions = g _1 <|> g _2 <|> g _3 <|> g _4
    where 
    g :: Lens' Positions Coord -> Maybe (a,Positions)
    g i = fmap (_2 %~ (\ix -> positions & i .~ ix)) $ f $ positions ^. i 
    
distanceTo :: Set Coord -> Coord -> Coord -> Maybe Int
distanceTo s a b = go 0 mempty (Set.singleton a)
    where
    go !acc !seen new 
      | null new = Nothing
      | b `Set.member` new = Just acc
      | otherwise = 
        let next = Set.intersection s (foldMap (Set.fromList . neighbors) new) Set.\\ seen
         in go (succ acc) (next <> seen) next

-- * Parsing
parseBoard :: String -> Board
parseBoard s = Board
    { openSquares = Set.fromList . map fst . filter (isOpenChar . snd) . assocs $ arr
    , keyLocs = Map.fromList keyList 
    , keyMaps = Map.fromList $ map swap keyList
    , doorLocs = Map.fromList doorList
    , doorMaps = Map.fromList $ map swap doorList
    , entryPoint = toPos $ map fst $ filter ((=='@') . snd) $ assocs arr
    }
    where
    arr = charArray s
    doorList = map (_2 %~ toLower) $ filter (isUpper . snd) $ assocs arr
    keyList = filter (isLower . snd) $ assocs arr
    
toPos [a,b,c,d] = (a,b,c,d)
fromPos (a,b,c,d) = [a,b,c,d]
    
isOpenChar :: Char -> Bool
isOpenChar c = isAlpha c || c == '.' || c == '@'

_ppr :: Board -> State -> String
_ppr board st = unlines
    [ [ fromMaybe (if ix `Set.member` openSquares board then '.' else '#') $ 
            Map.lookup ix keysRemaining 
        <|> fmap toUpper (Map.lookup ix doorsRemaining)
        <|> fmap (const '@') (Data.List.find (==ix) $ fromPos (st ^. pos))
      | x <- [x0..x1]
      , let ix = (x,y)
      ]
    | y <- [y0..y1]
    ]
    where
    x0 = pred $ minimum $ Set.map fst $ openSquares board
    x1 = succ $ maximum $ Set.map fst $ openSquares board
    y0 = pred $ minimum $ Set.map snd $ openSquares board
    y1 = succ $ maximum $ Set.map snd $ openSquares board
    doesntHaveKey = flip notMember (st ^. keys)
    keysRemaining = Map.filter doesntHaveKey (keyLocs board) 
    doorsRemaining = Map.filter doesntHaveKey (doorLocs board) 

-- * Utils
charArray :: String -> UArray Coord Char
charArray s = array ((0,0),(x1,y1))
    [ ((x,y),c)
    | (y,l) <- zip [0..y1] $ lines s
    , (x,c) <- zip [0..x1] l
    ]
    where
    y1 = pred $ length $ filter (not . null) $ lines s
    x1 = pred $ length $ head $ lines s

neighbors :: Coord -> [Coord]
neighbors (x,y) = 
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
-- * Ex
ex0,ex1,ex2,ex3 :: String
-- 8
ex0="#######\
\\n#a.#Cd#\
\\n##@#@##\
\\n#######\
\\n##@#@##\
\\n#cB#Ab#\
\\n#######"

-- 24
ex1="###############\
\\n#d.ABC.#.....a#\
\\n######@#@######\
\\n###############\
\\n######@#@######\
\\n#b.....#.....c#\
\\n###############"

-- 32
ex2 ="#############\
\\n#DcBa.#.GhKl#\
\\n#.###@#@#I###\
\\n#e#d#####j#k#\
\\n###C#@#@###J#\
\\n#fEbA.#.FgHi#\
\\n#############"

-- 72
ex3 ="#############\
\\n#g#f.D#..h#l#\
\\n#F###e#E###.#\
\\n#dCba@#@BcIJ#\
\\n#############\
\\n#nK.L@#@G...#\
\\n#M###N#H###.#\
\\n#o#m..#i#jk.#\
\\n#############"

-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
keys :: Lens' State KeySet
keys f_al9v (State x1_al9w x2_al9x)
  = (fmap (\ y1_al9y -> (State x1_al9w) y1_al9y)) (f_al9v x2_al9x)
{-# INLINE keys #-}
pos :: Lens' State Positions
pos f_al9z (State x1_al9A x2_al9B)
  = (fmap (\ y1_al9C -> (State y1_al9C) x2_al9B)) (f_al9z x1_al9A)
{-# INLINE pos #-}