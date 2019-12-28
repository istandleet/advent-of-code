{-# language LambdaCase #-} 
module Intcode (
    Program, Computer, initComputer,
    runToPause, PauseReason(..),
    interactSt, interactAscii
    ) where

import Control.Monad.State.Strict
import Control.Lens
import Data.Function
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

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
    
-- * Lenses
-- :set -ddump-splices
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
writeAt i value initialV = V.force newV
    where
    l = V.length initialV
    newV 
        | i < l = V.modify (\vec -> VM.write vec i value) initialV
        | value == 0 = initialV
        | i == l = initialV `V.snoc` value
        | otherwise = initialV V.++ (V.replicate (i-l) 0 `V.snoc` value)
        
interactSt :: [Int] -> State Computer [Int]
interactSt [] = getAllOutputs
interactSt (i:is) = state (feedOneInput i) >>= \b -> if b then interactSt is else do
    os <- getAllOutputs
    error $ unlines
        [ "Unexpected Output!" 
        , show os
        , "Remaining Input:"
        , show (i:is)
        ]
        
interactAscii :: String -> State Computer String
interactAscii = fmap (map toEnum) . interactSt . map fromEnum 

getAllOutputs :: State Computer [Int]
getAllOutputs = do
    mo <- state getOneOut
    case mo of 
        Nothing -> pure []
        Just o -> fmap (o:) getAllOutputs
        
-- | If the next I/O is not an output, returns the original comp
getOneOut :: Computer -> (Maybe Int, Computer)
getOneOut c = case runToPause c of 
    HasOutput (i,c') -> (Just i,c')
    -- Stopped -> error "Stopped while reading output"
    _ -> (Nothing,c)
    
feedOneInput :: Int -> Computer -> (Bool,Computer)
feedOneInput i c = case runToPause c of
    NeedsInput f -> (True,f i)
    Stopped -> error "Stopped while feeding input"
    _ -> (False,c)