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
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "11.txt" :: IO Program
    print $ p1 dat
    putStrLn $ ppr $ p2 dat


p1 :: Program -> Int
p1 = length . Set.fromList . init . map (^. currPos) . iterateMaybe step . initState

p2 :: Program -> Set Coord
p2 = _board . last . iterateMaybe step . initState'

initState :: Program -> State
initState p = State mempty (Computer 0 0 p) (0,0) U
initState' :: Program -> State
initState' p = State (Set.singleton (0,0)) (Computer 0 0 p) (0,0) U

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

-- * Intcode 
type Program = V.Vector Int
data Computer = Computer 
   { _nextIndex    :: {-# unpack #-} !Int
   , _relativeBase :: {-# unpack #-} !Int
   , _currVector   :: !Program
   } deriving (Eq,Show,Read)
   
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

ppr :: Set Coord -> String
ppr s = unlines 
    [   [if (x,y) `Set.member` s then blockChar else ' '
        |x<-[minimum $ Set.map fst s..maximum $ Set.map fst s]
        ]
    |y<-[minimum $ Set.map snd s..maximum $ Set.map snd s]
    ]
    
blockChar = '█'