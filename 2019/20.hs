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
    print $ p1 $ parseBoard s -- 568
    print $ p2 $ parseBoard s -- 6546

p1 :: Board -> Maybe Int
p1 b = Data.List.findIndex (Set.member $ exitPoint b) $ rangeTo b
p2 :: Board -> Maybe Int
p2 b = p2' b (0,exitPoint b)
p2' :: Board -> (Int,Coord) -> Maybe Int
p2' b rix = Data.List.findIndex (Set.member rix) $ rangeToR b
    
-- * Robot
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Board = Board
   { openSquares :: !(Set Coord)
   , portals :: !(Map Coord Coord)
   , entryPoint :: !Coord
   , exitPoint :: !Coord
   , outerBounds :: !(Coord,Coord)
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
    
rangeTo :: Board -> [Set Coord]
rangeTo board = go mempty (Set.singleton $ entryPoint board)
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' (new' Set.\\ acc')
        where 
        acc' = acc <> new
        new' = foldMap (validMoves board) new 

validMoves :: Board -> Coord -> Set Coord
validMoves board ix = levelMoves <> portalMoves
    where 
    levelMoves = Set.intersection (openSquares board) (Set.map (`move` ix) ds)
    portalMoves = maybe mempty Set.singleton (Map.lookup ix $ portals board) 
    ds = Set.fromList [minBound..maxBound]
    
rangeToR :: Board -> [Set (Int,Coord)]
rangeToR board = go mempty (Set.singleton $ (0,entryPoint board))
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' (new' Set.\\ acc')
        where 
        acc' = acc <> new
        new' = foldMap (uncurry $ validMovesR board) new 

validMovesR :: Board -> Int -> Coord -> Set (Int,Coord)
validMovesR board recursion ix = levelMoves <> portalMoves
    where 
    levelMoves = Set.map (recursion,) $ Set.intersection (openSquares board) (Set.map (`move` ix) ds)
    portalMoves = case Map.lookup ix $ portals board of
        Nothing -> mempty
        Just ix' -> if isOuterPortal board ix
            then if recursion == 0 then mempty else Set.singleton (recursion-1,ix')
            else Set.singleton (recursion+1,ix')
    ds = Set.fromList [minBound..maxBound]
    
isOuterPortal :: Board -> Coord -> Bool
isOuterPortal board (x,y) = x `elem` [x0,x1] || y `elem` [y0,y1]
    where ((x0,y0),(x1,y1)) = outerBounds board
    
-- * Parsing
parseBoard :: String -> Board
parseBoard s = Board
    { openSquares = openSpaces arr
    , portals     = pm
    , entryPoint  = fromJust $ lookup "AA" pi
    , exitPoint   = fromJust $ lookup "ZZ" pi
    , outerBounds = locateOuterBounds arr
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

locateOuterBounds :: UArray Coord Char -> (Coord,Coord)
locateOuterBounds arr = (minimum walls,maximum walls)
    where walls = map fst . filter ((== '#') . snd) . assocs $ arr
             
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

ex2="             Z L X W       C                 \
\\n             Z P Q B       K                 \
\\n  ###########.#.#.#.#######.###############  \
\\n  #...#.......#.#.......#.#.......#.#.#...#  \
\\n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \
\\n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \
\\n  #.###.#######.###.###.#.###.###.#.#######  \
\\n  #...#.......#.#...#...#.............#...#  \
\\n  #.#########.#######.#.#######.#######.###  \
\\n  #...#.#    F       R I       Z    #.#.#.#  \
\\n  #.###.#    D       E C       H    #.#.#.#  \
\\n  #.#...#                           #...#.#  \
\\n  #.###.#                           #.###.#  \
\\n  #.#....OA                       WB..#.#..ZH\
\\n  #.###.#                           #.#.#.#  \
\\nCJ......#                           #.....#  \
\\n  #######                           #######  \
\\n  #.#....CK                         #......IC\
\\n  #.###.#                           #.###.#  \
\\n  #.....#                           #...#.#  \
\\n  ###.###                           #.#.#.#  \
\\nXF....#.#                         RF..#.#.#  \
\\n  #####.#                           #######  \
\\n  #......CJ                       NM..#...#  \
\\n  ###.#.#                           #.###.#  \
\\nRE....#.#                           #......RF\
\\n  ###.###        X   X       L      #.#.#.#  \
\\n  #.....#        F   Q       P      #.#.#.#  \
\\n  ###.###########.###.#######.#########.###  \
\\n  #.....#...#.....#.......#...#.....#.#...#  \
\\n  #####.#.###.#######.#######.###.###.#.#.#  \
\\n  #.......#.......#.#.#.#.#...#...#...#.#.#  \
\\n  #####.###.#####.#.#.#.#.###.###.#.###.###  \
\\n  #.......#.....#.#...#...............#...#  \
\\n  #############.#.#.###.###################  \
\\n               A O F   N                     \
\\n               A A D   M                     "