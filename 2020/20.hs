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
import Control.Monad
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

main :: IO ()
main = do
    input <- readInput <$> readFile "20.txt"
    print $ part1 input

    seamonster <- readBoard <$> readFile "20-sea-monster.txt"
    putStrLn $ drawBoard seamonster
    let seamonstercoords = map fst $ filter snd $ assocs seamonster 

    print $ part2 seamonstercoords input

part1 :: Input -> Integer
part1 = product . map toInteger . Map.keys . Map.filter ((==2).length) . neighbors

-- part2 :: [Coord] -> Input -> [Int]
part2 seamonstercoords input = total - maximum (map (length . foldMap Set.fromList . dragons) manips)
    where
    total = length $ filter id $ elems compiled
    compiled = combine input
    (xmax,ymax) = snd $ bounds compiled
    manips = Data.List.subsequences [minBound..maxBound] :: [[Manipulation]]
    makesm :: [Manipulation] -> [Coord]
    makesm orient = map (applyManips (maximum $ map fst seamonstercoords) (maximum $ map snd seamonstercoords) orient) seamonstercoords
    dragons orient = 
        let deltas = makesm orient
            smx = maximum $ map fst deltas
            smy = maximum $ map snd deltas
         in [ coords
            | x0 <- [0..xmax-smx]
            , y0 <- [0..ymax-smy]
            , let coords = map (\(dx,dy) -> (x0+dx,y0+dy)) deltas
            , all (compiled!) coords
            ]


neighbors :: Input -> Map Tile (Map Tile ([Manipulation], [Manipulation]))
neighbors input = Map.fromList
    [ (t, Map.fromList $ mapMaybe (\(t',b') -> fmap (t',) $ guard (t /= t') *> listToMaybe (compatible' b b')) input) | (t,b) <- input]

combine :: Input -> Board
combine input = array bnds 
    [ go tx ty x y 
    | tx <- [0..width]
    , ty <- [0..height]
    , x <- [x0+1..x1-1]
    , y <- [y0+1..y1-1]
    ]
    where
    ns = neighbors input
    minput = Map.fromList input
    tps = tilePosition input
    ctps = Map.fromList $ map swap $ Map.toList tps
    orientations = Map.mapWithKey getneigh tps
        where 
        getneigh tile (tx,ty) 
            | ty < height = (ctps Map.! (tx,ty+1), fst $ ns Map.! tile Map.! (ctps Map.! (tx,ty+1)))
            | otherwise = (ctps Map.! (tx,ty-1), fst $ ns Map.! tile Map.! (ctps Map.! (tx,ty-1)))
    c = ctps Map.! (0,0)
    ((x0,y0),(x1,y1)) = bounds $ minput Map.! c
    (width,height) = fst $ Map.findMax ctps
    bnds = ((0,0),((width+1)*subwidth,(height+1)*subheight))
    subwidth = x1-x0-1
    subheight = y1-y0-1

    manips = manipAnalysis input
    go tx ty x y = 
        let tile = ctps Map.! (tx,ty)
            (x',y') = applyManips x1 y1 (manips Map.! tile) (x,y)
            coord = (subwidth*tx+x'-1, subheight*ty+y'-1)
         in (coord, (minput Map.! (ctps Map.! (tx,ty))) ! (x,y))

tilePosition :: Input -> Map Tile Coord
tilePosition input = let c = head corners in go (Map.singleton c (0,0)) mempty (Set.singleton c)
    where
    ns = fmap Map.keysSet (neighbors input) :: Map Tile (Set Tile)
    corners = Map.keys $ Map.filter ((==2).length) ns
    go acc visited nxt | null nxt = acc
    go acc visited nxt =
        let new = (Map.keysSet $ Map.filter (not . null . Set.intersection nxt) ns) Set.\\ visited'
            visited' = visited <> nxt
            acc' = foldl assign acc new
         in go acc' visited' new
    assign acc tile = 
        let myns = ns Map.! tile
            assigned = Map.elems $ Map.restrictKeys acc myns
            newcoord = case assigned of 
                [(a,b)] -> -- 1 assigned means an edge, so one of these should be zero
                    if a == 0 && b == 0 then (if length acc == 1 then (1,0) else (0,1))
                        else if a == 0 then (0,b+1) 
                        else if b == 0 then (a+1,0) 
                        else error "only 1 assigned on non edge"
                [(a,b),(a',b')] -> -- 1 assigned means an edge, so one of these should be zero
                    (max a a', max b b')
         in Map.insert tile newcoord acc

cornerAnalysis :: Input -> Map Tile (Coord, Coord) -- Top Left, Top Right
cornerAnalysis input = Map.mapWithKey go tps
    where
    tps = tilePosition input
    ctps = Map.fromList $ map swap $ Map.toList tps
    ns = neighbors input
    ((xmin,ymin),(xmax,ymax)) = bounds $ snd $ head input

    oppositeCorner (x,y) = (if x == xmin then xmax else xmin, if y == ymin then ymax else ymin)
    
    go tile (tx,ty) = case Map.lookup (tx,ty+1) ctps of
        Just t -> 
            let t0 = transformVia t (xmin,ymin)
                t9 = transformVia t (xmax,ymin)
             in case Map.lookup (tx+1,ty) ctps of 
                    Just l ->
                        let l0 = transformVia l (xmin,ymin)
                            l9 = transformVia l (xmax,ymin)
                         in if t0 == l0 || t0 == l9 then (t0, t9) else (t9, t0)
                    Nothing -> 
                        let r = ctps Map.! (tx-1,ty)
                            r0 = transformVia r (xmin,ymin)
                            r9 = transformVia r (xmax,ymin)
                         in if t0 == r0 || t0 == r9 then (t9, t0) else (t0, t9)
        Nothing -> 
            let b = ctps Map.! (tx,ty-1)
                b0 = transformVia b (xmin,ymin)
                b9 = transformVia b (xmax,ymin)
             in case Map.lookup (tx+1,ty) ctps of 
                    Just l ->
                        let l0 = transformVia l (xmin,ymin)
                            l9 = transformVia l (xmax,ymin)
                         in if b0 == l0 || b9 == l0 then (l9, oppositeCorner l0) else (l0, oppositeCorner l9)
                    Nothing -> 
                        let r = ctps Map.! (tx-1,ty)
                            r0 = transformVia r (xmin,ymin)
                            r9 = transformVia r (xmax,ymin)
                         in if b0 == r0 || b9 == r0 then (oppositeCorner r0, r9) else (oppositeCorner r9, r0)
        where 
        transformVia o = applyManips xmax ymax (fst $ ns Map.! tile Map.! o)


manipAnalysis :: Input -> Map Tile [Manipulation] -- Top Left, Top Right
manipAnalysis input = Map.mapMaybe (uncurry toManip) $ cornerAnalysis input
    where
    ((xmin,ymin),(xmax,ymax)) = bounds $ snd $ head input
    toManip :: Coord -> Coord -> Maybe [Manipulation]
    toManip tl tr = Data.List.find (\ms -> makef ms tl == (xmax,ymax) && makef ms tr == (xmin,ymax)) manips
    makef = applyManips xmax ymax
    manips = Data.List.subsequences [minBound..maxBound] :: [[Manipulation]]

type Tile = Int
type Coord = (Int,Int)
type Board = UArray Coord Bool

compatible :: Board -> Board -> Maybe (Int,Int)
compatible a b = listToMaybe 
    [ (o1,o2)
    | (o1,e1) <- zip [0..] edgelist
    , (o2,e2) <- zip [0..] edgelist
    , and $ zipWith (\ix1 ix2 -> a!ix1 == b!ix2) e1 e2
    ]
    where
    edgelist = edges a

edges arr = foldMap (\l -> [l, reverse l]) 
          $ [map (,y0) [x0..x1], map (,y1) [x0..x1]
            ,map (x0,) [y0..y1], map (x1,) [y0..y1]]
    where
    ((x0,y0),(x1,y1)) = bounds arr

compatible' :: Board -> Board -> [([Manipulation],[Manipulation])]
compatible' a b =
    [ (as,bs)
    | as <- manips
    , bs <- manips
    , all (\ix -> a!(makef as ix) == b!(makef bs ix)) edge
    ]
    where
    ((x0,y0),(xmax,ymax)) = bounds a
    edge = head $ edges a
    makef = applyManips xmax ymax
    manips = Data.List.subsequences [minBound..maxBound]

    flipVert (x,y) = (x,ymax-y)
    flipHori (x,y) = (xmax-x,y)

data Manipulation = Swap | Vert | Hori deriving (Eq, Show, Ord, Bounded, Enum)

manip _ _    Swap = swap 
manip _ ymax Vert = \(x,y) -> (x,ymax-y)
manip xmax _ Hori = \(x,y) -> (xmax-x,y)

applyManips xmax ymax = foldr (.) id . map (manip xmax ymax)

-- * Parsing
type Input = [(Int, Board)]

readInput :: String -> Input
readInput "" = []
readInput s = (tile,board) : readInput (unlines rest)
    where
    ((header:boardls),rest) = splitAt 12 $ lines s
    tile= read $ init $ last $ words header
    board = readBoard $ unlines boardls


readBoard :: String -> Board
readBoard s = array bnds
    [ ((x,y), c == '#')
    | (y,l) <- zip [0..] ls
    , (x,c) <- zip [0..] l
    ]
    where
    ls = takeWhile (not.null) $ lines s
    bnds = ((0,0),(pred $ length $ head ls, pred $ length ls))

-- * Utils
drawBoard :: Board -> String
drawBoard arr = unlines $
    [ [if arr!(x,y) then '#' else ' ' | x <- [x0..x1]]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds arr

reference = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0)]