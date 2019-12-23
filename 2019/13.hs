{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language TemplateHaskell #-}
module Main where

import Control.Lens
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

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "13.txt" :: IO Program
    print $ p1 dat
    let dat' = V.fromList $ 2 : tail (V.toList dat)
    print $ p2 dat' -- 13282
    
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

-- * Intcode 
type Program = V.Vector Int
data Computer = Computer 
   { _nextIndex    :: {-# unpack #-} !Int
   , _relativeBase :: {-# unpack #-} !Int
   , _currVector   :: !Program
   } deriving (Eq,Show,Read)
initComputer :: Program -> Computer
initComputer = Computer 0 0 
   
data ReadOrWrite = Read | Write deriving (Eq,Ord,Show,Enum,Bounded)
data ParameterMode = Position | Immediate | Relative
     deriving (Eq,Ord,Show,Enum,Bounded)

readParameterMode :: Int -> ParameterMode
readParameterMode 0 = Position
readParameterMode 1 = Immediate
readParameterMode 2 = Relative
readParameterMode n = error $ "Unrecognized ParameterMode: " ++ show n

getVal :: ReadOrWrite -> ParameterMode -> Int -> Computer -> Int
getVal rw pm i s = case (rw,pm) of 
    (Read ,Position ) -> v ! (v ! i)
    (Read ,Immediate) -> v ! i
    (Read ,Relative ) -> v ! ((v ! i) + _relativeBase s)
    (Write,Position ) -> v ! i
    (Write,Relative ) -> (v ! i) + _relativeBase s
    where
    v = _currVector s
    (!) :: Program -> Int -> Int
    (!) v' i' = fromMaybe 0 $ v' V.!? i'

data Opcode =
     Add    !ParameterMode !ParameterMode !ParameterMode
   | Mult   !ParameterMode !ParameterMode !ParameterMode
   | Stop
   | Input  !ParameterMode
   | Output !ParameterMode
   | JumpIfTrue  !ParameterMode !ParameterMode 
   | JumpIfFalse !ParameterMode !ParameterMode 
   | LessThan    !ParameterMode !ParameterMode !ParameterMode
   | Equals      !ParameterMode !ParameterMode !ParameterMode
   | RelativeBaseOffset !ParameterMode
     deriving (Eq,Ord,Show)

readOpcode :: Int -> Opcode
readOpcode n = case opc of
    1  -> Add  a b c
    2  -> Mult a b c
    3  -> Input  a
    4  -> Output a
    5  -> JumpIfTrue  a b 
    6  -> JumpIfFalse a b 
    7  -> LessThan    a b c
    8  -> Equals      a b c
    9  -> RelativeBaseOffset a
    99 -> Stop
    _ -> error $ "Unrecognized Opcode: " ++ show n
    where
    (pms,opc) = n `divMod` 100
    (r ,a) = readParameterMode <$> pms `divMod` 10
    (r',b) = readParameterMode <$> r `divMod` 10
    (_ ,c) = readParameterMode <$> r' `divMod` 10

data PauseReason = 
     Stopped 
   | NeedsInput (Int -> Computer) 
   | HasOutput (Int,Computer)
   
isStopped :: PauseReason -> Bool
isStopped Stopped = True
isStopped _ = False
    
runOp :: Computer -> Opcode -> Either PauseReason Computer
runOp s = \case 
    Add  p1 p2 p3 -> Right $ s & nextIndex %~ (+4) & currVector %~ 
        let a = getVal Read  p1 (i+1) s
            b = getVal Read  p2 (i+2) s
            j = getVal Write p3 (i+3) s
         in writeAt j (a + b)
    Mult p1 p2 p3 -> Right $ s & nextIndex %~ (+4) & currVector %~ 
        let a = getVal Read  p1 (i+1) s
            b = getVal Read  p2 (i+2) s
            j = getVal Write p3 (i+3) s
         in writeAt j (a * b)
    JumpIfTrue p1 p2 -> Right $ s & nextIndex %~ 
        let a = getVal Read p1 (i+1) s
            b = getVal Read p2 (i+2) s
         in if a /= 0 then const b else (+3)
    JumpIfFalse p1 p2 -> Right $ s & nextIndex %~ 
        let a = getVal Read p1 (i+1) s
            b = getVal Read p2 (i+2) s
         in if a == 0 then const b else (+3)
    LessThan p1 p2 p3 -> Right $ s & nextIndex %~ (+4) & currVector %~ 
        let a = getVal Read p1 (i+1) s
            b = getVal Read p2 (i+2) s
            j = getVal Write p3 (i+3) s
         in writeAt j (if a < b then 1 else 0)
    Equals p1 p2 p3 -> Right $ s & nextIndex %~ (+4) & currVector %~ 
        let a = getVal Read p1 (i+1) s
            b = getVal Read p2 (i+2) s
            j = getVal Write p3 (i+3) s
         in writeAt j (if a == b then 1 else 0)
    RelativeBaseOffset p1 -> Right $ s & nextIndex %~ (+2) & relativeBase %~ (+getVal Read p1 (i+1) s)
    Input p1 -> Left $ NeedsInput $ \input -> 
                s & nextIndex %~ (+2) & currVector %~ 
                    let j = getVal Write p1 (i+1) s
                     in writeAt j input
    Output p1 -> Left $ HasOutput 
                ( getVal Read p1 (i+1) s
                , s & nextIndex %~ (+2)
                )
    Stop  -> Left Stopped
    where
    i = s ^. nextIndex
    
getOp :: Computer -> Opcode
getOp s = readOpcode $ (s ^. currVector) V.! (s ^. nextIndex)
   
stepComputer :: Computer -> Either PauseReason Computer
stepComputer s = runOp s $ getOp s

runToPause :: Computer -> PauseReason
runToPause = either id runToPause . stepComputer 
    
-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
-- makeLenses ''Computer
currVector :: Lens' Computer Program
currVector f_aK6I (Computer x1_aK6J x2_aK6K x3_aK6L)
  = (fmap (\ y1_aK6M -> ((Computer x1_aK6J) x2_aK6K) y1_aK6M))
      (f_aK6I x3_aK6L)
{-# INLINE currVector #-}
nextIndex :: Lens' Computer Int
nextIndex f_aK6N (Computer x1_aK6O x2_aK6P x3_aK6Q)
  = (fmap (\ y1_aK6R -> ((Computer y1_aK6R) x2_aK6P) x3_aK6Q))
      (f_aK6N x1_aK6O)
{-# INLINE nextIndex #-}
relativeBase :: Lens' Computer Int
relativeBase f_aK6S (Computer x1_aK6T x2_aK6U x3_aK6V)
  = (fmap (\ y1_aK6W -> ((Computer x1_aK6T) y1_aK6W) x3_aK6V))
      (f_aK6S x2_aK6U)
{-# INLINE relativeBase #-}

-- * Utils    
writeAt :: Int -> Int -> Program -> Program
writeAt i value initialV 
    | i < l = V.modify (\vec -> VM.write vec i value) initialV
    | value == 0 = initialV
    | i == l = initialV `V.snoc` value
    | otherwise = initialV V.++ (V.replicate (i-l) 0 `V.snoc` value)
    where
    l = V.length initialV

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