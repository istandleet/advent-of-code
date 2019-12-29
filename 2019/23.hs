module Main where

import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Intcode

main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "23.txt" :: IO Program
    print $ p1 dat
    print $ p2 dat
    
data Network = Network
    { _nat :: Maybe Message
    , _lastNat :: Maybe Message
    , _repeatedNat :: Maybe Message
    , _net :: Vector (Vector Message, Computer)
    }

type Message = (Int,Int)
    
initNetwork :: Program -> Network
initNetwork = Network Nothing Nothing Nothing 
            . V.imap (\i comp -> (V.singleton (i,-1),comp)) 
            . V.replicate 50 . initComputer 
    
p1 :: Program -> Int
p1 = snd . head . mapMaybe _nat . iterate step . initNetwork
p2 :: Program -> Int
p2 = snd . head . mapMaybe _repeatedNat . iterate step . initNetwork

step :: Network -> Network
step n = 
    let b = checkIdle n
        n' = processNetwork n
        b' = checkIdle n'
     in if b && b' then bootIdle n' else n'

processNetwork :: Network -> Network
processNetwork = execState $ mapM_ processIx [0..49]
 
checkIdle :: Network -> Bool
checkIdle = all (null . fst) . _net 

bootIdle :: Network -> Network
bootIdle n = n 
           & sendMessage 0 m
           & lastNat .~ Just m
           & repeatedNat .~ if _lastNat n == _nat n then _nat n else Nothing
    where Just m = _nat n 

sendMessage :: Int -> Message -> Network -> Network
sendMessage 255 m = nat .~ Just m
sendMessage i m = net %~ modifyAt i (_1 %~ (`V.snoc` m))

processIx :: Int -> State Network ()
processIx ix = do
    (is,comp) <- (V.! ix) . _net <$> get
    let (os,comp') = processInputs is comp
    mapM_ (modify . uncurry sendMessage) os
    modify $ net %~ writeAt ix (mempty,comp') 
    
processInputs :: Vector Message -> Computer -> (Vector (Int,Message),Computer)
processInputs v 
    | null v    = runState $ V.fromList . finalize <$> interactSt [-1]
    | otherwise = runState $ V.fromList . finalize . concat <$> mapM (\(x,y) -> interactSt [x,y]) v
    where
    finalize [] = []
    finalize (i:x:y:is) = (i,(x,y)):finalize is
    
    
writeAt  ix v = V.modify $ \vec -> VM.write vec ix v
modifyAt ix f = V.modify $ \vec -> VM.read vec ix >>= \v -> VM.write vec ix (f v)

-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Network
lastNat :: Lens' Network (Maybe Message)
lastNat f_ajXl (Network x1_ajXm x2_ajXn x3_ajXo x4_ajXp)
  = (fmap
       (\ y1_ajXq -> (((Network x1_ajXm) y1_ajXq) x3_ajXo) x4_ajXp))
      (f_ajXl x2_ajXn)
{-# INLINE lastNat #-}
nat :: Lens' Network (Maybe Message)
nat f_ajXr (Network x1_ajXs x2_ajXt x3_ajXu x4_ajXv)
  = (fmap
       (\ y1_ajXw -> (((Network y1_ajXw) x2_ajXt) x3_ajXu) x4_ajXv))
      (f_ajXr x1_ajXs)
{-# INLINE nat #-}
net :: Lens' Network (Vector (Vector Message, Computer))
net f_ajXx (Network x1_ajXy x2_ajXz x3_ajXA x4_ajXB)
  = (fmap
       (\ y1_ajXC -> (((Network x1_ajXy) x2_ajXz) x3_ajXA) y1_ajXC))
      (f_ajXx x4_ajXB)
{-# INLINE net #-}
repeatedNat :: Lens' Network (Maybe Message)
repeatedNat f_ajXD (Network x1_ajXE x2_ajXF x3_ajXG x4_ajXH)
  = (fmap
       (\ y1_ajXI -> (((Network x1_ajXE) x2_ajXF) y1_ajXI) x4_ajXH))
      (f_ajXD x3_ajXG)
{-# INLINE repeatedNat #-}