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
    s <- readFile "20.txt"
    print $ p1 $ parseBoard s

p1 :: Board -> Maybe Int
p1 b = Data.List.findIndex (Set.member $ exitPoint b) $ rangeTo b
    
-- * Robot
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Board = Board
   { openSquares :: !(Set Coord)
   , portals :: !(Map Coord Coord)
   , entryPoint :: !Coord
   , exitPoint :: !Coord
   } deriving (Eq, Show)

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

pathTo :: Set Coord -> Coord -> Coord -> [(Dir,Coord)]
pathTo _ a b | a == b = []
pathTo s a b = 
    let neighbors = filter ((`Set.member` s) . snd) $ map (\d -> (d,move d a)) [minBound..maxBound]
     in case find ((==b) . snd) neighbors of
                Just p -> [p]
                Nothing -> go (Set.fromList $ map snd neighbors) $ map pure neighbors 
    where
    go !seen paths = 
        let paths' = 
              [ (d,c'):path
              | path <- paths
              , let (_,c) = head path
              , d <- [minBound..maxBound]
              , let c' = move d c
              , c' `Set.member` s
              , c' `Set.notMember` seen
              ]
            seen' = seen <> Set.fromList (map (snd . head) paths')
         in case find ((==b) . snd . head) paths' of
                Just p -> reverse p
                Nothing -> if null paths' then error $ "No path found between " ++ show a ++ " and " ++ show b ++ " in " ++ show s
                            else go seen' paths'

ranges :: Set Coord -> Coord -> [Set Coord]
ranges s = go mempty . Set.singleton 
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' new'
        where 
        acc' = acc <> new
        new' = Set.intersection s $ foldMap (\c -> Set.map (`move` c) ds) new Set.\\ acc'
    ds = Set.fromList [minBound..maxBound]
    
rangeTo :: Board -> [Set Coord]
rangeTo board = go mempty (Set.singleton $ entryPoint board)
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' (new' Set.\\ acc')
        where 
        acc' = acc <> new
        new' = foldMap (validMoves board) new 

validMoves :: Board -> Coord -> Set Coord
validMoves board ix = 
       maybe mempty Set.singleton (Map.lookup ix $ portals board) 
    <> Set.intersection (openSquares board) (Set.map (`move` ix) ds)
    where ds = Set.fromList [minBound..maxBound]
    
-- * Parsing
parseBoard :: String -> Board
parseBoard s = Board
    { openSquares = openSpaces arr
    , portals     = pm
    , entryPoint  = fromJust $ lookup "AA" pi
    , exitPoint   = fromJust $ lookup "ZZ" pi
    }
    where
    arr = charArray s
    pi :: [(String,Coord)]
    pi = portalInfo arr
    pm :: Map Coord Coord
    pm = foldMap go 
       $ map (map snd) 
       $ Data.List.groupBy ((==) `on` fst) 
       $ tail . init 
       $ Data.List.sort $ pi
        where
        go :: [Coord] -> Map Coord Coord
        go [a,b] = Map.fromList [(a,b),(b,a)]

openSpaces :: UArray Coord Char -> Set Coord
openSpaces = Set.fromList . map fst . filter ((== '.') . snd) . assocs 

portalInfo :: UArray Coord Char -> [(String,Coord)]
portalInfo arr = mapMaybe go . filter (isAlpha . snd) $ assocs arr
    where
    isOpen ix = inRange (bounds arr) ix && arr ! ix == '.'
    go (ix,c) = case Data.List.find (isOpen . (`move` ix)) [minBound..maxBound] of
        Nothing -> Nothing
        Just d -> Just $
            let s = (if d `elem` [U,R] then id else reverse) [c,arr ! move (oppositeDir d) ix]
             in (s,move d ix)

-- * Utils
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)
    
blockChar :: Char
blockChar = '█'

charArray :: String -> UArray Coord Char
charArray s = array ((0,0),(x1,y1))
    [ ((x,y),c)
    | (y,l) <- zip [0..] $ lines s
    , (x,c) <- zip [0..] l
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
ex = "         A           \
\\n         A           \
\\n  #######.#########  \
\\n  #######.........#  \
\\n  #######.#######.#  \
\\n  #######.#######.#  \
\\n  #######.#######.#  \
\\n  #####  B    ###.#  \
\\nBC...##  C    ###.#  \
\\n  ##.##       ###.#  \
\\n  ##...DE  F  ###.#  \
\\n  #####    G  ###.#  \
\\n  #########.#####.#  \
\\nDE..#######...###.#  \
\\n  #.#########.###.#  \
\\nFG..#########.....#  \
\\n  ###########.#####  \
\\n             Z       \
\\n             Z       "

ex1="                   A               \
\\n                   A               \
\\n  #################.#############  \
\\n  #.#...#...................#.#.#  \
\\n  #.#.#.###.###.###.#########.#.#  \
\\n  #.#.#.......#...#.....#.#.#...#  \
\\n  #.#########.###.#####.#.#.###.#  \
\\n  #.............#.#.....#.......#  \
\\n  ###.###########.###.#####.#.#.#  \
\\n  #.....#        A   C    #.#.#.#  \
\\n  #######        S   P    #####.#  \
\\n  #.#...#                 #......VT\
\\n  #.#.#.#                 #.#####  \
\\n  #...#.#               YN....#.#  \
\\n  #.###.#                 #####.#  \
\\nDI....#.#                 #.....#  \
\\n  #####.#                 #.###.#  \
\\nZZ......#               QG....#..AS\
\\n  ###.###                 #######  \
\\nJO..#.#.#                 #.....#  \
\\n  #.#.#.#                 ###.#.#  \
\\n  #...#..DI             BU....#..LF\
\\n  #####.#                 #.#####  \
\\nYN......#               VT..#....QG\
\\n  #.###.#                 #.###.#  \
\\n  #.#...#                 #.....#  \
\\n  ###.###    J L     J    #.#.###  \
\\n  #.....#    O F     P    #.#...#  \
\\n  #.###.#####.#.#####.#####.###.#  \
\\n  #...#.#.#...#.....#.....#.#...#  \
\\n  #.#####.###.###.#.#.#########.#  \
\\n  #...#.#.....#...#.#.#.#.....#.#  \
\\n  #.###.#####.###.###.#.#.#######  \
\\n  #.#.........#...#.............#  \
\\n  #########.###.###.#############  \
\\n           B   J   C               \
\\n           U   P   P               "