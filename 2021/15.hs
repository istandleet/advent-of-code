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
    putStrLn $ drawSearch $ last $ biastar' input
    go input
    let !b = mkBoard2 input
    print "go"
    go b

    where 
    go b =  do
        go1 "Bidij" solve'' b
        go1 "A*"    solve'  b
        go1 "Dij"   solve   b 
    go1 name f b = do 
        start <- getCurrentTime
        let !y = f b 
        end <- getCurrentTime
        print (name, y, end `diffUTCTime` start)



search :: (Board -> [Array Coord AStar]) -> Board -> ([Array Coord AStar],Array Coord AStar)
search f board = 
    let (l,h) = bounds board 
        (s,bor) = break (\m -> isDone $ m ! h) $ f board
     in (s, head bor)

part1, part2 :: Board -> Int
part1 = solve
part2 = solve'' . mkBoard2

solve board = go $ dijkstra' board
    where 
    (from, to) = bounds board
    go :: [Array Coord AStar] -> Int
    go (a:as) = case a ! to of
        Done n -> n
        _ -> go as

solve' board = go $ astar' board
    where 
    (from, to) = bounds board
    go :: [Array Coord AStar] -> Int
    go (a:as) = case a ! to of
        Done n -> n
        _ -> go as

solve'' board = go $ last $ biastar' board
    where 
    (from, to) = bounds board
    go a = case a ! to of
        Done n -> n

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

-- type FullMap = Array (Coord,Coord) Int
-- fullMap :: Board -> FullMap
-- fullMap board = array bnds
--     [ ((from,to), go from to)
--     | (from,to) <- range bnds
--     ]
--     where 
--     bnds = let (l,h) = bounds board in ((l,l),(h,h))
--     go from to = if from == to then 0 else 1

data AStar = Far | Near {-# unpack #-} !Int | Done {-# unpack #-} !Int deriving (Eq, Show, Ord)
isDone (Done n) = True
isDone _ = False
nearValue (Near curr) = Just curr
nearValue _ = Nothing

start :: Board -> Coord -> Array Coord AStar
start board from = listArray (bounds board) $ map (uncurry go) $ assocs board
    where
    go ix v | ix == from = Near 0
    go ix v = Far
    ((xmin,ymin),(xmax,ymax)) = bounds board
    
dijkstra' board = let (l,h) = bounds board in map snd $ dijkstra board l
dijkstra :: Board -> Coord -> [(Coord,Array Coord AStar)]
dijkstra board from = iter mempty (Set.singleton from) $ start board from
    where 
    -- iter :: Set Coord -> Array Coord AStar -> [(Coord,Array Coord AStar)]
    iter !done !near !acc | null near = []
    iter !done !near !acc = 
        let next = minimumBy (compare `on` (acc !)) near
            Near v = acc ! next
            updates = 
                [ (j,t)
                | j <- neighbors board next
                , j `Set.notMember` done
                , t <- if j `Set.notMember` near then [Near (v+board!j)] 
                        else case acc ! j of
                                Near low -> 
                                    let low' = v + board ! j 
                                     in if low' < low then [Near low'] else []
                ]
            acc' = acc // ((next, Done v):updates)
            done' = Set.insert next done
            near' = (near <> Set.fromList (map fst updates)) Set.\\ done'
         in (next,acc') : iter done' near' acc'

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

biastar' board = let (l,h) = bounds board in biastar board l h 
biastar :: Board -> Coord -> Coord -> [Array Coord AStar]
biastar board from to = go (dijkstra board from) (dijkstra board to)
    where
    go ((i,b):bs) ((j,c):cs) 
        | isDone (c!i) = [merge b c // [(to,Done $ finish i (b!i) (c!i))]]
        | isDone (b!j) = [merge b c // [(to,Done $ finish j (b!j) (c!j))]]
        | otherwise =  merge b c : go bs cs 
    merge :: Array Coord AStar -> Array Coord AStar -> Array Coord AStar
    merge a b = listArray (bounds board) $ zipWith max (elems a) (elems b)
    fromDone (Done e) = e
    finish i (Done tl) (Done br) = tl + br - board ! i + board ! to

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

drawBoard :: Show a => Array (Int,Int) a  -> String
drawBoard m = init $ unlines
    [   Data.List.intercalate "\t" [ show (m !(x,y))
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds m

drawBoard' :: Board  -> String
drawBoard' m = init $ unlines
    [   concat [ show (m !(x,y))
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds m

-- mapM_ (putStrLn . drawSearch) $ take 100 $ dijkstra input (0,0)
drawSearch :: Array Coord AStar  -> String
drawSearch m = unlines
    [ 
        [ case m ! (x,y) of 
            Far -> '.'
            Near _ -> '-'
            Done _ -> 'X'

        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds m

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')