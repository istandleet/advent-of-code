{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

main :: IO ()
main = do
    s <- readFile "08.txt"
    input <- fmap V.fromList $ either fail pure $ parseOnly parseLines $ T.pack s
    print $ part1 input
    print $ part2 input

part1 :: Vector Action -> Int
part1 = findDup . iterate step . OpState 0 0
    where
    findDup = _accumulator
            . head
            . findDuplicatesBy _current

data Action = 
    Jmp Int
  | Nop Int
  | Acc Int
  deriving (Show,Eq)

data OpState = OpState 
   { _accumulator :: {- unpack -} !Int
   , _current :: {- unpack -} !Int
   , _instructions :: Vector Action
   } deriving (Show,Eq)

step :: OpState -> OpState
step o@OpState{..} = stepAction (_instructions V.! _current) o

stepAction :: Action -> OpState -> OpState
stepAction (Nop _) o = o & current %~ succ
stepAction (Acc n) o = o & current %~ succ & accumulator %~ (+n)
stepAction (Jmp n) o = o & current %~ (+n)

part2 :: Vector Action -> [Int]
part2 =  map terminalValue . filter terminates . part2permutations

swapJmpNop (Nop n) = Just (Jmp n)
swapJmpNop (Acc _) = Nothing
swapJmpNop (Jmp n) = Just (Nop n)

part2permutations :: Vector Action -> [Vector Action]
part2permutations v = map go $ V.toList $ V.imapMaybe (\i val -> (,) i <$> swapJmpNop val) v
    where
    go (i,val) = V.modify (\mv -> VM.write mv i val) v

terminalValue :: Vector Action -> Int
terminalValue = run . iterate step . OpState 0 0
    where
    run = _accumulator . head
        . dropWhile (\o -> _current o < length (_instructions o))
        . takeWhile (\o -> _current o <= length (_instructions o))

terminates :: Vector Action -> Bool
terminates = null . findDup . iterate step . OpState 0 0
    where
    findDup = findDuplicatesBy _current 
            . takeWhile (\o -> _current o < length (_instructions o))

-- Parsing
type Line = Action
parseLines :: P.Parser [Line]
parseLines = (parseAction `sepBy1'` endOfLine) <* skipSpace <* endOfInput

parseAction :: Parser Action
parseAction = choice [pjmp,pnop,pacc] 
    where
    pjmp = fmap Jmp $ "jmp " *> parseInt
    pnop = fmap Nop $ "nop " *> parseInt
    pacc = fmap Acc $ "acc " *> parseInt
    parseInt = signed decimal
    
-- * Util
findDuplicates :: Ord a => [a] -> [a]
findDuplicates = findDuplicatesBy id
             
findDuplicatesBy :: Ord a => (b -> a) -> [b] -> [b]
findDuplicatesBy f = map snd . filter fst . map fst . Data.List.scanl' folder ((False,undefined),mempty) 
    where 
    folder (_,s) x = 
        let y = f x
         in ((y `Set.member` s,x),Set.insert y s)

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''OpState 
accumulator :: Lens' OpState Int
accumulator f_akhP (OpState x1_akhQ x2_akhR x3_akhS)
    = (fmap (\ y1_akhT -> ((OpState y1_akhT) x2_akhR) x3_akhS))
        (f_akhP x1_akhQ)
{-# INLINE accumulator #-}
current :: Lens' OpState Int
current f_akhU (OpState x1_akhV x2_akhW x3_akhX)
    = (fmap (\ y1_akhY -> ((OpState x1_akhV) y1_akhY) x3_akhX))
        (f_akhU x2_akhW)
{-# INLINE current #-}
instructions :: Lens' OpState (Vector Action)
instructions f_akhZ (OpState x1_aki0 x2_aki1 x3_aki2)
    = (fmap (\ y1_aki3 -> ((OpState x1_aki0) x2_aki1) y1_aki3))
        (f_akhZ x3_aki2)
{-# INLINE instructions #-}