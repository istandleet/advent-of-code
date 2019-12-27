{-# language LambdaCase #-} 
module Main where

import Control.Lens
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.List

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "13.txt" :: IO Program
    print $ p1 dat -- 280
    let dat' = V.fromList $ 2 : tail (V.toList dat)
    print $ p2 dat' -- 13298
    
p1 :: Program -> Int
p1 = length . Map.filter (==Block) . _board .  buildBoard . initState
p2 :: Program -> Int
p2 = _score . last . iterateMaybe play . buildBoard . initState

-- * Robot
data State = State
   { _board    :: !Board
   , _score    :: {-# unpack #-} !Int
   , _ballPos  :: {-# unpack #-} !Coord
   , _paddPos  :: {-# unpack #-} !Coord
   , _computer :: !Computer
   } deriving (Eq,Show,Read)
initState = State mempty 0 (-1,0) (-1,0) . initComputer 
drawState :: State  -> String
drawState st = unlines
    ["Score: " ++ show (_score st)
    ,""
    ,drawBoard $ st ^. board
    ,replicate 40 '-'
    ]

type Coord = (Int,Int)
data Tile = EmptyT | Wall | Block | HPaddle | Ball deriving (Eq,Show,Ord,Read,Enum,Bounded)
tileChar = \case
    EmptyT -> ' '
    Wall  -> '|'
    Block -> blockChar
    HPaddle -> '_'
    Ball -> '*'

type Board = Map Coord Tile
    
buildBoard :: State -> State
buildBoard = last . iterateMaybe go
    where
    go st = case runToPause (st ^. computer) of
        Stopped -> Nothing
        NeedsInput f -> Nothing
        HasOutput (-1,_) -> Nothing
        HasOutput (x,comp) -> case runToPause comp of 
            HasOutput (y,comp) -> case runToPause comp of 
                HasOutput (t,comp) -> Just $ 
                    st & computer .~ comp 
                       & changes x y t
    changes x y t = 
        let tile = toEnum t 
            changeBoard = board %~ Map.insert (x,y) tile
        in case tile of
            Ball    -> changeBoard . (ballPos .~ (x,y))
            HPaddle -> changeBoard . (paddPos .~ (x,y))
            _ -> changeBoard
            
play :: State -> Maybe State
play st = case runToPause (st ^. computer) of
    Stopped -> Nothing
    NeedsInput f -> Just $ st & computer .~ f input
    HasOutput (x,comp) -> case runToPause comp of 
        HasOutput (y,comp) -> case runToPause comp of 
            HasOutput (t,comp) -> Just $ 
                st & computer .~ comp 
                   & changes x y t
    where
    input = signum (fst (_ballPos st) - fst (_paddPos st))
    changes (-1) 0 t = score .~ t
    changes x y t = 
        let tile = toEnum t 
            changeBoard = board %~ Map.insert (x,y) tile
        in case tile of
            Ball    -> changeBoard . (ballPos .~ (x,y))
            HPaddle -> changeBoard . (paddPos .~ (x,y))
            _ -> changeBoard 

gameStates :: State -> [State]
gameStates = map head . Data.List.groupBy ((==) `on` _ballPos) . iterateMaybe play 
watchGame :: State -> IO ()
watchGame = mapM_ (putStrLn . drawState) . gameStates
            

-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
ballPos :: Lens' State Coord
ballPos f_aMKd (State x1_aMKe x2_aMKf x3_aMKg x4_aMKh x5_aMKi)
  = (fmap
       (\ y1_aMKj
          -> ((((State x1_aMKe) x2_aMKf) y1_aMKj) x4_aMKh) x5_aMKi))
      (f_aMKd x3_aMKg)
{-# INLINE ballPos #-}
board :: Lens' State Board
board f_aMKk (State x1_aMKl x2_aMKm x3_aMKn x4_aMKo x5_aMKp)
  = (fmap
       (\ y1_aMKq
          -> ((((State y1_aMKq) x2_aMKm) x3_aMKn) x4_aMKo) x5_aMKp))
      (f_aMKk x1_aMKl)
{-# INLINE board #-}
computer :: Lens' State Computer
computer f_aMKr (State x1_aMKs x2_aMKt x3_aMKu x4_aMKv x5_aMKw)
  = (fmap
       (\ y1_aMKx
          -> ((((State x1_aMKs) x2_aMKt) x3_aMKu) x4_aMKv) y1_aMKx))
      (f_aMKr x5_aMKw)
{-# INLINE computer #-}
paddPos :: Lens' State Coord
paddPos f_aMKy (State x1_aMKz x2_aMKA x3_aMKB x4_aMKC x5_aMKD)
  = (fmap
       (\ y1_aMKE
          -> ((((State x1_aMKz) x2_aMKA) x3_aMKB) y1_aMKE) x5_aMKD))
      (f_aMKy x4_aMKC)
{-# INLINE paddPos #-}
score :: Lens' State Int
score f_aMKF (State x1_aMKG x2_aMKH x3_aMKI x4_aMKJ x5_aMKK)
  = (fmap
       (\ y1_aMKL
          -> ((((State x1_aMKG) y1_aMKL) x3_aMKI) x4_aMKJ) x5_aMKK))
      (f_aMKF x2_aMKH)
{-# INLINE score #-}

-- * Utils    
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = fix $ \rec x -> x : maybe [] rec (f x)

drawBoard :: Board  -> String
drawBoard m = unlines 
    [   [tileChar $ m Map.! (x,y)
        |x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    (x0,y0) = fst $ Map.findMin m
    (x1,y1) = fst $ Map.findMax m
    
blockChar = '█'