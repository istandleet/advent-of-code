{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Concurrent.Async
import Control.Applicative
-- import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
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

main :: IO ()
main = do
    input <- getInput <$> readFile "15.txt" :: IO Board
    putStrLn $ drawSearch input $ last $ bidijkstra' input
    go input
    let !b = mkBoard2 input
    go b
    where 
    go b = 
        let g f = time f b >>= print
         in mapM_ g [solve, solveBloom, solveBloom', solveBidir, solveBidirMany]
    time f b = do 
        !start <- getCurrentTime
        !y <- pure $ f b
        !end <- getCurrentTime
        return (y, end `diffUTCTime` start)

solve board = go $ dijkstra' board
    where 
    (from, to) = bounds board
    go = (Map.! to) . head . dropWhile (Map.notMember to)

solveBloom board = go $ map snd $ dijkstraMany board from
    where 
    (from, to) = bounds board
    go = (Map.! to) . head . dropWhile (Map.notMember to)

solveBloom' board = go $ dibloom board from
    where 
    (from, to) = bounds board
    go = fst . head . dropWhile (Set.notMember to . snd)

solveBidir board = (Map.! to) $ last $ bidijkstra' board
    where (from, to) = bounds board

solveBidirMany board = (Map.! to) $ last $ bidiMany' board
    where (from, to) = bounds board

mkBoard2 :: Board -> Board
mkBoard2 board = array bnds
    [ ((x,y), go x y)
    | (x,y) <- range bnds
    ]
    where
    go x y = 
        let (p,x') = x `divMod` (xmax+1)
            (q,y') = y `divMod` (xmax+1)
            base = board ! (x',y')
            new = base + q + p
         in if new > 9 then new `mod` 9 else new
    (xmax,ymax) = snd $ bounds board
    bnds = ((0,0),(5*(xmax+1)-1,5*(ymax+1)-1))

type Coord = (Int,Int)
type Board = UArray Coord Int
    
dijkstra' board = let (l,h) = bounds board in map snd $ dijkstra board l
dijkstra :: Board -> Coord -> [(Coord,Map Coord Int)]
dijkstra board from = iter (Map.singleton from 0) mempty
    where 
    -- iter :: Set Coord -> Array Coord AStar -> [(Coord,Array Coord AStar)]
    iter !yellow !green | null yellow = []
    iter !yellow !green = 
        let Min (v,next) = Map.foldMapWithKey (\k v -> Min (v,k)) yellow
            updates = 
                [ (j,t)
                | j <- neighbors board next
                , j `Map.notMember` green
                , t <- case Map.lookup j yellow of 
                        Nothing -> [v+board!j]
                        Just low -> let low' = v + board ! j in if low' < low then [low'] else []
                ]
            green' = Map.insert next v green
            yellow' = Map.delete next yellow <> Map.fromList updates
         in (next,green') : iter yellow' green'

dijkstraMany :: Board -> Coord -> [(Set Coord,Map Coord Int)]
dijkstraMany board from = iter (Map.singleton from 0) mempty
    where 
    -- iter :: Set Coord -> Array Coord AStar -> [(Coord,Array Coord AStar)]
    iter !yellow !green | null yellow = []
    iter !yellow !green = 
        let Mins (v, next) = getMins yellow
            nexts = Set.fromList next
            updates = 
                [ (j,t)
                | j <- Set.toList $ foldMap (Set.fromList . neighbors board) next Set.\\ nexts
                , j `Map.notMember` green
                , t <- case Map.lookup j yellow of 
                        Nothing -> [v+board!j]
                        Just low -> let low' = v + board ! j in if low' < low then [low'] else []
                ]
            green' = green <> Map.fromSet (const v) nexts
            yellow' = Map.withoutKeys yellow nexts <> Map.fromList updates
         in (nexts,green') : iter yellow' green'
    
getMins :: (Ord a, Bounded a, Ord c) => Map c a -> Mins a [c]
getMins = Map.foldMapWithKey (\k v -> Mins (v,[k]))

newtype Mins a c = Mins (a,c) deriving Eq
instance (Ord a, Semigroup c) => Semigroup (Mins a c) where 
    Mins (a,xs) <> Mins (b, ys) = case compare a b of 
        LT -> Mins (a,xs)
        EQ -> Mins (a,xs <> ys)
        GT -> Mins (b,ys)
instance (Ord a, Bounded a, Monoid c) => Monoid (Mins a c) where 
    mempty = Mins (maxBound, mempty)



dibloom :: Board -> Coord -> [(Int, Set Coord)]
dibloom board from = iter (IntMap.singleton 0 $ Set.singleton from) mempty
    where 
    -- iter :: Set Coord -> Array Coord AStar -> [(Coord,Array Coord AStar)]
    iter !yellow !green | null yellow = []
    iter !yellow !green = 
        let ((v,next), rest) = IntMap.deleteFindMin yellow
            actual = next Set.\\ green
         in if null actual 
             then iter rest green 
             else let green' = green <> actual
                      updates = foldMap (Set.fromList . neighbors board) actual
                         Set.\\ (actual <> green)
                      yellow' = foldr (\ix -> IntMap.insertWith (<>) (v + board ! ix) (Set.singleton ix)) rest $ updates
                   in (v, actual) : iter yellow' green'

bidijkstra' board = let (l,h) = bounds board in bidijkstra board l h 
bidijkstra :: Board -> Coord -> Coord -> [Map Coord Int]
bidijkstra board from to = go (dijkstra board from) (dijkstra board to)
    where
    go ((i,b):bs) ((j,c):cs) 
        | Map.member i c = [Map.insert to (finish i (b Map.! i) (c Map.! i)) $ b <> c]
        | Map.member j b = [Map.insert to (finish j (b Map.! j) (c Map.! j)) $ b <> c]
        | otherwise = b <> c : go bs cs 
    finish i tl br = tl + br - board ! i + board ! to


bidiMany' board = let (l,h) = bounds board in bidiMany board l h 
bidiMany :: Board -> Coord -> Coord -> [Map Coord Int]
bidiMany board from to = go (dijkstraMany board from) (dijkstraMany board to)
    where
    finish i tl br = tl + br - board ! i + board ! to
    go ((is,b):bs) ((js,c):cs) 
        | not $ null matchingc = 
            let Mins (vc,(i:_)) = getMins matchingc 
             in [Map.insert to (finish i (b Map.! i) vc) $ b <> c]
        | not $ null matchingb = 
            let Mins (vb,(j:_)) = getMins matchingb
             in [Map.insert to (finish j vb (c Map.! j)) $ b <> c]
        | otherwise = b <> c : go bs cs 
        where 
        matchingc = Map.restrictKeys c is
        matchingb = Map.restrictKeys b js
{-
astar' board = let (l,h) = bounds board in map snd $ astar board l h
astar :: Board -> Coord -> Coord -> [(Coord,Array Coord AStar)]
astar board from to = iter $ start board from
    where 
    iter :: Array Coord AStar -> [(Coord,Array Coord AStar)]
    iter acc | isDone (acc ! to) = []
    iter acc = 
        let (c,v) = next acc
            acc' = set c v acc
         in (c,acc') : iter acc'

    next :: Array Coord AStar -> (Coord, Int)
    next = Data.List.minimumBy (compare `on` uncurry yellow) . mapMaybe (\(ix,v) -> (ix,)<$>nearValue v) . assocs

    yellow ix v = lowerbounds ! ix + v

    lowerbounds :: Board
    lowerbounds = array (bounds board)
        [ (ix, manhattanDist ix to)
        | ix <- range (bounds board)
        ]

    set :: Coord -> Int -> Array Coord AStar -> Array Coord AStar
    set ix v curr = curr // ((ix, Done v):updates)
        where 
        updates = 
            [ (j,t)
            | j <- neighbors board ix
            , t <- case curr ! j of
                Done _ -> []
                Near low -> 
                    let low' = v + board ! j 
                    in if low' < low then [Near low'] else []
                Far -> [Near (v+board!j)]
            ]
-}

neighbors :: Board -> Coord -> [Coord]
neighbors d (x,y) = filter (inRange $ bounds d)
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]

getInput s = array ((0,0),(xmax, ymax))
    [ ((x,y),read [c])
    | (y,l) <- zip [0..] $ filter (not . null) $ lines s
    , (x,c) <- zip [0..] l
    ] 
    where
    ymax = pred $ length $ filter (not . null) $ lines s
    xmax = pred $ length $ head $ lines s

-- mapM_ (putStrLn . drawSearch) $ take 100 $ dijkstra input (0,0)
drawSearch :: Board -> Map Coord Int -> String
drawSearch b m = unlines
    [ 
        [ case Map.lookup (x,y) m of 
            Nothing -> '.'
            -- Near _ -> '-'
            Just _ -> 'X'

        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds b

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')