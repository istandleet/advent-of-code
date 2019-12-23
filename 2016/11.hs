{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language NoMonomorphismRestriction #-}
{-# language TransformListComp #-}
{-# language DeriveGeneric #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Hashable
import Data.Foldable
import Data.Ord
import Data.Maybe
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Algorithms.Merge as VSort
import GHC.Generics (Generic)
import GHC.Exts (groupWith,the)
import System.Environment

import Data.Char
import Data.Attoparsec.Text as P hiding (take)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "11"
    args <- getArgs
    case args of 
        ["1" ] -> part1M  input1 >>= print
        ["2" ] -> part1M  input2 >>= print
        ["1'"] -> part1M' input1 >>= print
        ["2'"] -> part1M' input2 >>= print
        _      -> part1M  exampl >>= print
    
__maxTries  = 80
__numFloors = 4
floors = Set.fromList [1..__numFloors]
    
type Positions = UVector.Vector (Int,Int)
data Pos = Pos 
    { elevatorFloor :: {-#unpack#-} !Int
    , positions     :: {-#unpack#-} !Positions
    } deriving (Eq, Ord, Show)
    
instance Hashable Pos where
    hashWithSalt s Pos{..} = hashWithSalt s (elevatorFloor, bimap UVector.toList UVector.toList $ UVector.unzip positions)
    
part1M :: Pos -> IO Int
part1M start = go 0 (Set.singleton start, Set.singleton start)
    where
    go !i _ | i > __maxTries = putStrLn "didn't finish" *> return 0
    go !i (poss,seen) = if any finished poss then return i else do
        putStrLn $ unwords [show i, "\t- Total positions:", show $ length poss]
        go (succ i) $ stepPositions poss seen
    
    finished = UVector.all (\(a,b) -> a == __numFloors && b == __numFloors) . positions

stepPositions :: Set Pos -> Set Pos -> (Set Pos, Set Pos)
stepPositions !ps !seen = 
    let ps' = foldMap possibleMoves ps
     in (ps' Set.\\ seen,seen <> ps') 
     
part1M' :: Pos -> IO Int
part1M' start = go 0 (Set.singleton start, IntSet.singleton $ hash start)
    where
    go !i _ | i > __maxTries = putStrLn "didn't finish" *> return 0
    go !i (poss,seen) = if any finished poss then return i else do
        putStrLn $ unwords [show i, "\t- Total positions:", show $ length poss]
        go (succ i) $ stepPositions' poss seen
    
    finished = UVector.all (\(a,b) -> a == __numFloors && b == __numFloors) . positions
    
stepPositions' :: Set Pos -> IntSet -> (Set Pos, IntSet)
stepPositions' !ps !seen = 
    let ps' = foldMap possibleMoves ps
     in (Set.filter (\p -> hash p `IntSet.notMember` seen) ps',IntSet.fromList (map hash $ Set.toList ps') <> seen) 
    
possibleMoves :: Pos -> Set Pos
possibleMoves pos 
    -- | shouldntMoveDown = Set.fromList $ moves (floor + 1) pos
    | otherwise        = Set.fromList $ moves (floor + 1) pos <> moves (floor - 1) pos
    where
    floor = elevatorFloor pos
    shouldntMoveDown = not (UVector.any (\(a,b) -> a < floor || b < floor) $ positions pos)

-- a micro and a non paired gen can never travel together: 
-- either the ms current floor has its g or the next floor does, but not both
moves :: Int -> Pos -> [Pos]
moves nextfloor _ | nextfloor `Set.notMember` floors= []
moves nextfloor pos@Pos{..} = map (Pos nextfloor . sort . (positions UVector.//)) newpos
    where
    newpos = micromoves nextfloor pos
          <> bothmoves nextfloor pos  
          <> foldMap (genmoves nextfloor pos) ixs
    ixs = [0..UVector.length positions-1]
    
        
type Moves = [(Int,(Int,Int))]
micromoves :: Int -> Pos -> [Moves]
micromoves nextfloor pos@Pos{..} = solo <> duo
    where
    cango = filter (miccanmove nextfloor pos) ixs
    
    ixs = [0..UVector.length positions-1]
    genpos j = snd (positions UVector.! j)
    
    solo = map (\i -> [(i,(nextfloor,genpos i))]) cango
    duo = [ [(i,(nextfloor, genpos i))
            ,(j,(nextfloor, genpos j))
            ]
          | i <- cango
          , j <- cango
          , i < j
          ]
          
miccanmove :: Int -> Pos -> Int -> Bool
miccanmove nextfloor Pos{..} i = mcango
    where
    ixs = [0..UVector.length positions-1]
    genpos j = snd (positions UVector.! j)
    micpos j = fst (positions UVector.! j)
    
    mon = micpos i == elevatorFloor
    numnextgs = UVector.length $ UVector.filter ((==nextfloor).snd) positions
    
    -- mcango if it's on the floor, and the next floor it either contains it's gen or doesn't have a gen
    mcango = mon && (genpos i == nextfloor || numnextgs == 0)
        
genmoves :: Int -> Pos -> Int -> [Moves]
genmoves nextfloor Pos{..} i 
    | not gcango = []
    | otherwise  = solo <> duo
    where
    ixs = [0..UVector.length positions-1]
    genpos j = snd (positions UVector.! j)
    micpos j = fst (positions UVector.! j)
    
    mon   = micon i
    gon   = genon i
    micon j = micpos j == elevatorFloor
    genon j = genpos j == elevatorFloor
    micro = micpos i
    
    numgs  = length $ filter ((==elevatorFloor).genpos) ixs
    nextms = filter (\i -> micpos i == nextfloor) ixs
    
    -- gcango if it's on the floor, and it wouldn't leave behind an unpaired m, plus some extra dependencies
    gcango = gon 
    -- extra concerns: the next floor doesn't have any ms, or if it's taking the g of the only unpaired m
    solo = [ [(i,(micro,nextfloor))]
           | not mon || numgs == 1 -- not an unpaired m
           , all (\j -> genpos j == nextfloor) nextms -- no unpaired ms above
           ]
    duo = [ [(i,(micro,nextfloor))
            ,(j,(micpos j,nextfloor))
            ]
          | not mon || numgs == 2 -- not an unpaired m
          , j <- [i+1..UVector.length positions-1]
          , genon j
          , not (micon j) -- jm not here
         || numgs == 2 -- taking all the gs
          ]

-- i don't think you ever need to move two things down?
-- bothmoves nextfloor Pos{..} _ = []
bothmoves :: Int -> Pos -> [Moves]
bothmoves nextfloor Pos{..} = 
    [[(i,(nextfloor,nextfloor))]
    | no_unprotected_next_mics
    , i <- ixs
    , micon i
    , genon i
    ]
    where
    no_unprotected_next_mics = UVector.all (\(m,g) -> m == nextfloor --> g == nextfloor) positions
    
    ixs = [0..UVector.length positions-1]
    
    genpos j = snd (positions UVector.! j)
    micpos j = fst (positions UVector.! j)
    
    micon j = micpos j == elevatorFloor
    genon j = genpos j == elevatorFloor
            
exampl :: Pos
exampl = Pos 1 $ sort $ UVector.fromList [(1,2),(1,3)]

input1 :: Pos
input1 = Pos 1 $ sort $ UVector.fromList [(1,1),(1,1),(3,2),(2,2),(2,2)]
input2 :: Pos
input2 = Pos 1 $ sort $ UVector.fromList [(1,1),(1,1),(1,1),(1,1),(3,2),(2,2),(2,2)]


-- * Utils
sort :: Positions -> Positions
sort = UVector.modify VSort.sort

infixr 1 -->
(-->) :: Bool -> Bool -> Bool
False --> _ = True
True  --> b = b

{-
-- example
0       - Total positions: 1
1       - Total positions: 1
2       - Total positions: 2
3       - Total positions: 12
4       - Total positions: 18
5       - Total positions: 28
6       - Total positions: 28
7       - Total positions: 46
8       - Total positions: 28
9       - Total positions: 23
10      - Total positions: 11
11
-}
{-
-- input1
0       - Total positions: 1
1       - Total positions: 2
2       - Total positions: 14
3       - Total positions: 34
4       - Total positions: 63
5       - Total positions: 120
6       - Total positions: 130
7       - Total positions: 199
8       - Total positions: 214
9       - Total positions: 318
10      - Total positions: 360
11      - Total positions: 542
12      - Total positions: 732
13      - Total positions: 1430
14      - Total positions: 1523
15      - Total positions: 3001
16      - Total positions: 3060
17      - Total positions: 4365
18      - Total positions: 4108
19      - Total positions: 4363
20      - Total positions: 3777
21      - Total positions: 3118
22      - Total positions: 2688
23      - Total positions: 1861
24      - Total positions: 1527
25      - Total positions: 1032
26      - Total positions: 682
27      - Total positions: 486
28      - Total positions: 264
29      - Total positions: 181
30      - Total positions: 81
31      - Total positions: 59
32      - Total positions: 28
33      - Total positions: 20
34      - Total positions: 10
35      - Total positions: 3
36      - Total positions: 2
37
-}
{-
-- input2
0       - Total positions: 1
1       - Total positions: 4
2       - Total positions: 28
3       - Total positions: 160
4       - Total positions: 212
5       - Total positions: 698
6       - Total positions: 868
7       - Total positions: 3922
8       - Total positions: 1984
9       - Total positions: 3432
10      - Total positions: 3260
11      - Total positions: 7727
12      - Total positions: 6185
13      - Total positions: 30264
14      - Total positions: 25233
15      - Total positions: 123493
16      - Total positions: 125324
17      - Total positions: 578015
18      - Total positions: 526462
19      - Total positions: 1665299
20      - Total positions: 1372480
21      - Total positions: 2784881
-}