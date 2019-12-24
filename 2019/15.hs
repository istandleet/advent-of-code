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
    dat <- read . (\s -> "["++s++"]") <$> readFile "15.txt" :: IO Program
    let st = initState $ dat
    putStrLn $ drawState $ exploreDirection U st
    putStrLn $ drawState $ exploreCurrentSquare st
    let fullBoard = last $ iterateMaybe exploreSpace st
    putStrLn $ drawState fullBoard
    let Just o = _oxygen fullBoard
    print $ length $ pathTo (_visited fullBoard) (0,0) o
    print $ pred $ length $ ranges (_visited fullBoard) o
    
p1 = go . last . iterateMaybe exploreSpace . initState
    where go st = length $ pathTo (_visited st) (0,0) (fromJust $ _oxygen st)

-- * Robot
data State = State
   { _board    :: !Board
   , _position :: {-# unpack #-} !Coord
   , _explored :: !(Set Coord)
   , _visited  :: !(Set Coord) -- open squares
   , _oxygen   :: !(Maybe Coord)
   , _computer :: !Computer
   } deriving (Eq,Show,Read)
initState = State mempty (0,0) mempty (Set.singleton (0,0)) Nothing . initComputer 

type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)
type Board = Set Coord

move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ pred
    L -> _1 %~ succ
    R -> _1 %~ pred
    D -> _2 %~ succ
oppositeDir :: Dir -> Dir
oppositeDir = \case
    U -> D
    L -> R
    R -> L
    D -> U
    
dirInt :: Dir -> Int
dirInt = \case
    U -> 1
    D -> 2 
    L -> 3
    R -> 4
    
exploreSpace :: State -> Maybe State
exploreSpace s | _explored s == _visited s = Nothing
exploreSpace s = Just $ if _position s `Set.member` _explored s then moveToNewSquare s else exploreCurrentSquare s
    
exploreCurrentSquare :: State -> State
exploreCurrentSquare s = foldr' exploreDirection s [minBound..maxBound] 
                       & explored %~ Set.insert (_position s) 

exploreDirection :: Dir -> State -> State
exploreDirection d st = case runToPause (st ^. computer) of
    NeedsInput f -> case runToPause $ f $ dirInt d of 
        HasOutput (o,comp) -> case o of 
            0 -> st & computer .~ comp & board %~ Set.insert targetSquare
            1 -> st & computer .~ comp 
               & visited %~ Set.insert targetSquare 
               & position %~ move d 
               & brainlessStep (oppositeDir d)
            2 -> st & computer .~ comp 
               & visited %~ Set.insert targetSquare 
               & position %~ move d 
               & brainlessStep (oppositeDir d) 
               & oxygen .~ Just targetSquare
    HasOutput (o,_) -> error $ "Unexpected Output: " ++ show o
    Stopped -> error "Unexpected Stop :O"
    where
    targetSquare = move d $ _position st
    
brainlessStep :: Dir -> State -> State -- when you are stepping to a square you know is open
brainlessStep d st = case runToPause (st ^. computer) of
    NeedsInput f -> case runToPause $ f $ dirInt d of 
        HasOutput (0,_) -> error $ "Target space not open: " ++ show (move d $ _position st)
        HasOutput (_,comp) -> st & computer .~ comp & position %~ move d

moveToNewSquare :: State -> State
moveToNewSquare st = 
    let e = _explored st
        v = _visited  st
        p' = Set.findMin $ v Set.\\ e
     in moveTo p' st
     
moveTo :: Coord -> State -> State
moveTo p' st = 
    let p = _position st
        v = _visited  st
        path = pathTo v p p'
     in foldl' (flip brainlessStep) st $ map fst path
        
pathTo :: Set Coord -> Coord -> Coord -> [(Dir,Coord)]
pathTo _ a b | a == b = []
pathTo s a b = 
    let neighbors = filter ((`Set.member` s) . snd) $ map (\d -> (d,move d a)) [minBound..maxBound]
     in case find ((==b) . snd) neighbors of
                Just p -> [p]
                Nothing -> go (Set.fromList $ map snd neighbors) $ map pure neighbors 
    where
    go !seen paths = 
        let paths' = 
              [ (d,c'):path
              | path <- paths
              , let (_,c) = head path
              , d <- [minBound..maxBound]
              , let c' = move d c
              , c' `Set.member` s
              , c' `Set.notMember` seen
              ]
            seen' = seen <> Set.fromList (map (snd . head) paths')
         in case find ((==b) . snd . head) paths' of
                Just p -> reverse p
                Nothing -> if null paths' then error $ "No path found between " ++ show a ++ " and " ++ show b ++ " in " ++ show s
                            else go seen' paths'

ranges :: Set Coord -> Coord -> [Set Coord]
ranges s = go mempty . Set.singleton 
    where
    go !acc !new | null new = []
    go !acc !new = new : go acc' new'
        where 
        acc' = acc <> new
        new' = Set.intersection s $ foldMap (\c -> Set.map (`move` c) ds) new Set.\\ acc'
    ds = Set.fromList [minBound..maxBound]

-- ** Draw
drawState :: State  -> String
drawState m = unlines 
    [   [ if _oxygen m == Just c then 'O' 
     else if _position m == c then 'D'
     else if c `Set.member` _board m then '#' 
     else if c `Set.member` _visited m then ' ' 
     else '?'
        |x<-[x0..x1]
        , let c = (x,y)
        ]
    | y <- [y0..y1]
    ]
    where
    squares = _board m <> _visited m 
    x0 = minimum $ Set.map fst squares
    x1 = maximum $ Set.map fst squares
    y0 = minimum $ Set.map snd squares
    y1 = maximum $ Set.map snd squares

-- ** Lenses
-- :set -ddump-splices
-- makeLenses ''State
board :: Lens' State Board
board
  f_azUA
  (State x1_azUB x2_azUC x3_azUD x4_azUE x5_azUF x6_azUG)
  = (fmap
       (\ y1_azUH
          -> (((((State y1_azUH) x2_azUC) x3_azUD) x4_azUE) x5_azUF)
               x6_azUG))
      (f_azUA x1_azUB)
{-# INLINE board #-}
computer :: Lens' State Computer
computer
  f_azUI
  (State x1_azUJ x2_azUK x3_azUL x4_azUM x5_azUN x6_azUO)
  = (fmap
       (\ y1_azUP
          -> (((((State x1_azUJ) x2_azUK) x3_azUL) x4_azUM) x5_azUN)
               y1_azUP))
      (f_azUI x6_azUO)
{-# INLINE computer #-}
explored :: Lens' State (Set Coord)
explored
  f_azUQ
  (State x1_azUR x2_azUS x3_azUT x4_azUU x5_azUV x6_azUW)
  = (fmap
       (\ y1_azUX
          -> (((((State x1_azUR) x2_azUS) y1_azUX) x4_azUU) x5_azUV)
               x6_azUW))
      (f_azUQ x3_azUT)
{-# INLINE explored #-}
oxygen :: Lens' State (Maybe Coord)
oxygen
  f_azUY
  (State x1_azUZ x2_azV0 x3_azV1 x4_azV2 x5_azV3 x6_azV4)
  = (fmap
       (\ y1_azV5
          -> (((((State x1_azUZ) x2_azV0) x3_azV1) x4_azV2) y1_azV5)
               x6_azV4))
      (f_azUY x5_azV3)
{-# INLINE oxygen #-}
position :: Lens' State Coord
position
  f_azV6
  (State x1_azV7 x2_azV8 x3_azV9 x4_azVa x5_azVb x6_azVc)
  = (fmap
       (\ y1_azVd
          -> (((((State x1_azV7) y1_azVd) x3_azV9) x4_azVa) x5_azVb)
               x6_azVc))
      (f_azV6 x2_azV8)
{-# INLINE position #-}
visited :: Lens' State (Set Coord)
visited
  f_azVe
  (State x1_azVf x2_azVg x3_azVh x4_azVi x5_azVj x6_azVk)
  = (fmap
       (\ y1_azVl
          -> (((((State x1_azVf) x2_azVg) x3_azVh) y1_azVl) x5_azVj)
               x6_azVk))
      (f_azVe x4_azVi)
{-# INLINE visited #-}

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
    
blockChar = '█'

-- makeLenses ''State