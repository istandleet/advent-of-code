{-# language LambdaCase #-} 
module Main where

import Control.Lens
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "11.txt" :: IO Program
    print $ p1 dat -- 2268
    putStrLn $ ppr $ p2 dat -- CEPKZJCR


p1 :: Program -> Int
p1 = length . Set.fromList . init . map (^. currPos) . iterateMaybe step . initState

p2 :: Program -> Set Coord
p2 = _board . last . iterateMaybe step . initState'

initState :: Program -> State
initState p = State mempty (initComputer p) (0,0) U
initState' :: Program -> State
initState' p = State (Set.singleton (0,0)) (initComputer p) (0,0) U

-- * Robot
data State = State
   { _board    :: !Board
   , _computer :: !Computer
   , _currPos  :: {-# unpack #-} !Coord
   , _currDir  :: !Dir
   } deriving (Eq,Show,Read)

type Board = Set Coord -- The white coordinates
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord)
turn :: Bool -> Dir -> Dir
turn isright = \case
    U -> if isright then R else L
    L -> if isright then U else D
    R -> if isright then D else U
    D -> if isright then L else R
move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ succ
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ pred

step :: State -> Maybe State
step st = case runToPause (st ^. computer) of
    Stopped -> Nothing
    NeedsInput f -> Just $ st & computer .~ f input
    HasOutput (paintNum,comp) -> case runToPause comp of 
        HasOutput (turnNum,comp') -> Just $ 
            let d' = turn (turnNum == 1) $ st ^. currDir
             in st & computer .~ comp' 
                   & board %~ (if paintNum == 1 then Set.insert else Set.delete) (st ^. currPos)
                   & currDir .~ d' 
                   & currPos %~ move d' 
    where
    input = if (st ^. currPos) `Set.member` (st ^. board) then 1 else 0

-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
board :: Lens' State Board
board f_auYa (State x1_auYb x2_auYc x3_auYd x4_auYe)
  = (fmap (\ y1_auYf -> (((State y1_auYf) x2_auYc) x3_auYd) x4_auYe))
      (f_auYa x1_auYb)
{-# INLINE board #-}
computer :: Lens' State Computer
computer f_auYg (State x1_auYh x2_auYi x3_auYj x4_auYk)
  = (fmap (\ y1_auYl -> (((State x1_auYh) y1_auYl) x3_auYj) x4_auYk))
      (f_auYg x2_auYi)
{-# INLINE computer #-}
currDir :: Lens' State Dir
currDir f_auYm (State x1_auYn x2_auYo x3_auYp x4_auYq)
  = (fmap (\ y1_auYr -> (((State x1_auYn) x2_auYo) x3_auYp) y1_auYr))
      (f_auYm x4_auYq)
{-# INLINE currDir #-}
currPos :: Lens' State Coord
currPos f_auYs (State x1_auYt x2_auYu x3_auYv x4_auYw)
  = (fmap (\ y1_auYx -> (((State x1_auYt) x2_auYu) y1_auYx) x4_auYw))
      (f_auYs x3_auYv)
{-# INLINE currPos #-}

-- * Utils
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)

ppr :: Set Coord -> String
ppr s = unlines $ reverse
    [   [if (x,y) `Set.member` s then blockChar else ' '
        |x<-[minimum $ Set.map fst s..maximum $ Set.map fst s]
        ]
    |y<-[minimum $ Set.map snd s..maximum $ Set.map snd s]
    ]
    
blockChar = '█'