{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
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
    input <- readFile "19.txt"
    let cubes = map readLine $ filter (not . null) $ lines input
    print $ part1 cubes
    print $ part2 cubes

part1, part2 :: Input -> Int
part1 = sum . map score
part2 cs = 4

type Input = [Blueprint]

data Blueprint = Blueprint
   { num          :: {-# unpack #-} !Int
   , orecost      :: {-# unpack #-} !Int
   , claycost     :: {-# unpack #-} !Int
   , obsidiancost :: {-# unpack #-} !(Int, Int) -- ore, clay
   , geodecost    :: {-# unpack #-} !(Int, Int) -- ore, obsidian
   } deriving (Show, Eq, Ord)

score :: Blueprint -> Int
score bp = num bp * maxScore' bp initBoard

maxScore :: Blueprint -> Board -> Int -> Int
maxScore bp board remaining = fin $ iterate step start !! pred remaining
    where 
    start = [board]
    step = prune . foldMap (evolve bp)
    fin = maximum . map (_geode . mine)


maxScore' :: Blueprint -> Board -> Int
maxScore' bp board = 
    let mid = Set.toList $ Set.fromList $ iterate step start !! 16
        end = iterate (foldMap $ evolveEnd bp) mid !! 7
     in maximum $ map (_geode . mine) end
    where 
    start = [board]
    step = prune . foldMap (evolve bp)

-- maxScore :: Blueprint -> Board -> Int -> Int
-- maxScore bp board remaining = fin $ iterate step start !! pred remaining
--     where 
--     start = Set.singleton board
--     step = prune . foldMap (Set.fromList . evolve bp)
--     fin = maximum . map (_geode . mine) . Set.toList

evolve :: Blueprint -> Board -> [Board]
evolve bp board | canBuyGeode bp board = [buyGeode bp $ mine board]
evolve bp@Blueprint{..} board = concat
    [ [board']
    , [ buyOre bp board' 
      | canBuyOre bp board
      && maximum [claycost, fst obsidiancost, fst geodecost] > board ^. orebots
      ]
    , [buyClay bp board' | canBuyClay bp board]
    , [buyObsidian bp board' | canBuyObsidian bp board]
    ]
    where board' = mine board

evolveEnd :: Blueprint -> Board -> [Board]
evolveEnd bp board | canBuyGeode bp board = [buyGeode bp $ mine board]
evolveEnd bp@Blueprint{..} board = concat
    [ [board']
    , [buyClay bp board' | canBuyClay bp board]
    , [buyObsidian bp board' | canBuyObsidian bp board]
    ]
    where board' = mine board

-- prune :: Set Board -> Set Board
prune :: [Board] -> [Board]
prune bs = filter (not . dominated) bs
    where 
    dominated b = any (`dominates` b) bs
    dominates b1 b2 = 
           b1 ^. ore >= b2 ^. ore 
        && b1 ^. clay >= b2 ^. clay 
        && b1 ^. obsidian >= b2 ^. obsidian 
        && b1 ^. geode >= b2 ^. geode
        && b1 ^. orebots >= b2 ^. orebots
        && b1 ^. claybots >= b2 ^. claybots
        && b1 ^. obsidianbots >= b2 ^. obsidianbots
        && b1 ^. geodebots >= b2 ^. geodebots
        && b1 /= b2

canBuyGeode :: Blueprint -> Board -> Bool
canBuyGeode Blueprint{..} b = fst geodecost <= b ^. ore && snd geodecost <= b ^. obsidian

buyGeode :: Blueprint -> Board -> Board
buyGeode Blueprint{..} b = b & ore -~ fst geodecost & obsidian -~ snd geodecost & geodebots +~ 1

canBuyObsidian :: Blueprint -> Board -> Bool
canBuyObsidian Blueprint{..} b = fst obsidiancost <= b ^. ore && snd obsidiancost <= b ^. clay

buyObsidian :: Blueprint -> Board -> Board
buyObsidian Blueprint{..} b = b & ore -~ fst obsidiancost & clay -~ snd obsidiancost & obsidianbots +~ 1

canBuyClay :: Blueprint -> Board -> Bool
canBuyClay Blueprint{..} b = claycost <= b ^. ore

buyClay :: Blueprint -> Board -> Board
buyClay Blueprint{..} b = b & ore -~ claycost & claybots +~ 1

canBuyOre :: Blueprint -> Board -> Bool
canBuyOre Blueprint{..} b = orecost <= b ^. ore

buyOre :: Blueprint -> Board -> Board
buyOre Blueprint{..} b = b & ore -~ orecost & orebots +~ 1

mine :: Board -> Board 
mine b = b 
       & ore +~ b ^. orebots 
       & clay +~ b ^. claybots 
       & obsidian +~ b ^. obsidianbots 
       & geode +~ b ^. geodebots

data Board = Board 
   { _ore           :: {-# unpack #-} !Int
   , _clay          :: {-# unpack #-} !Int
   , _obsidian      :: {-# unpack #-} !Int
   , _geode         :: {-# unpack #-} !Int
   , _orebots       :: {-# unpack #-} !Int
   , _claybots      :: {-# unpack #-} !Int
   , _obsidianbots  :: {-# unpack #-} !Int
   , _geodebots     :: {-# unpack #-} !Int
   } deriving (Show, Eq, Ord)

initBoard :: Board
initBoard = Board 0 0 0 0 1 0 0 0

-- * Parsing
-- "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
-- "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

readLine :: String -> Blueprint
readLine s = case words s of 
    ["Blueprint", numcol 
        , "Each", "ore", "robot", "costs", orecost, "ore."
        , "Each", "clay", "robot", "costs", claycost, "ore."
        , "Each", "obsidian", "robot", "costs", obore, "ore", "and", obclay, "clay."
        , "Each", "geode", "robot", "costs", geore, "ore", "and", geob, "obsidian."
        ] -> Blueprint
            { num = read $ init numcol
            , orecost = read orecost
            , claycost = read claycost
            , obsidiancost = (read obore, read obclay)
            , geodecost = (read geore, read geob)
            }

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Board
clay :: Lens' Board Int
clay
    f_aOBO
    (Board x1_aOBP x2_aOBQ x3_aOBR x4_aOBS x5_aOBT x6_aOBU x7_aOBV
            x8_aOBW)
    = (fmap
        (\ y1_aOBX
            -> (((((((Board x1_aOBP) y1_aOBX) x3_aOBR) x4_aOBS) x5_aOBT)
                    x6_aOBU)
                x7_aOBV)
                x8_aOBW))
        (f_aOBO x2_aOBQ)
{-# INLINE clay #-}
claybots :: Lens' Board Int
claybots
    f_aOBY
    (Board x1_aOBZ x2_aOC0 x3_aOC1 x4_aOC2 x5_aOC3 x6_aOC4 x7_aOC5
            x8_aOC6)
    = (fmap
        (\ y1_aOC7
            -> (((((((Board x1_aOBZ) x2_aOC0) x3_aOC1) x4_aOC2) x5_aOC3)
                    y1_aOC7)
                x7_aOC5)
                x8_aOC6))
        (f_aOBY x6_aOC4)
{-# INLINE claybots #-}
geode :: Lens' Board Int
geode
    f_aOC8
    (Board x1_aOC9 x2_aOCa x3_aOCb x4_aOCc x5_aOCd x6_aOCe x7_aOCf
            x8_aOCg)
    = (fmap
        (\ y1_aOCh
            -> (((((((Board x1_aOC9) x2_aOCa) x3_aOCb) y1_aOCh) x5_aOCd)
                    x6_aOCe)
                x7_aOCf)
                x8_aOCg))
        (f_aOC8 x4_aOCc)
{-# INLINE geode #-}
geodebots :: Lens' Board Int
geodebots
    f_aOCi
    (Board x1_aOCj x2_aOCk x3_aOCl x4_aOCm x5_aOCn x6_aOCo x7_aOCp
            x8_aOCq)
    = (fmap
        (\ y1_aOCr
            -> (((((((Board x1_aOCj) x2_aOCk) x3_aOCl) x4_aOCm) x5_aOCn)
                    x6_aOCo)
                x7_aOCp)
                y1_aOCr))
        (f_aOCi x8_aOCq)
{-# INLINE geodebots #-}
obsidian :: Lens' Board Int
obsidian
    f_aOCs
    (Board x1_aOCt x2_aOCu x3_aOCv x4_aOCw x5_aOCx x6_aOCy x7_aOCz
            x8_aOCA)
    = (fmap
        (\ y1_aOCB
            -> (((((((Board x1_aOCt) x2_aOCu) y1_aOCB) x4_aOCw) x5_aOCx)
                    x6_aOCy)
                x7_aOCz)
                x8_aOCA))
        (f_aOCs x3_aOCv)
{-# INLINE obsidian #-}
obsidianbots :: Lens' Board Int
obsidianbots
    f_aOCC
    (Board x1_aOCD x2_aOCE x3_aOCF x4_aOCG x5_aOCH x6_aOCI x7_aOCJ
            x8_aOCK)
    = (fmap
        (\ y1_aOCL
            -> (((((((Board x1_aOCD) x2_aOCE) x3_aOCF) x4_aOCG) x5_aOCH)
                    x6_aOCI)
                y1_aOCL)
                x8_aOCK))
        (f_aOCC x7_aOCJ)
{-# INLINE obsidianbots #-}
ore :: Lens' Board Int
ore
    f_aOCM
    (Board x1_aOCN x2_aOCO x3_aOCP x4_aOCQ x5_aOCR x6_aOCS x7_aOCT
            x8_aOCU)
    = (fmap
        (\ y1_aOCV
            -> (((((((Board y1_aOCV) x2_aOCO) x3_aOCP) x4_aOCQ) x5_aOCR)
                    x6_aOCS)
                x7_aOCT)
                x8_aOCU))
        (f_aOCM x1_aOCN)
{-# INLINE ore #-}
orebots :: Lens' Board Int
orebots
    f_aOCW
    (Board x1_aOCX x2_aOCY x3_aOCZ x4_aOD0 x5_aOD1 x6_aOD2 x7_aOD3
            x8_aOD4)
    = (fmap
        (\ y1_aOD5
            -> (((((((Board x1_aOCX) x2_aOCY) x3_aOCZ) x4_aOD0) y1_aOD5)
                    x6_aOD2)
                x7_aOD3)
                x8_aOD4))
        (f_aOCW x5_aOD1)
{-# INLINE orebots #-}