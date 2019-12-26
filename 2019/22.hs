{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-} 
{-# language DeriveFunctor #-}
module Main where

import Control.Lens
import Control.Applicative
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.List
import Test.QuickCheck

import Data.Attoparsec.Text hiding (D,take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    is <- getInput
    let is' = map (fmap fromIntegral) is
    print $ p1 10007 is 2019 -- 8191
    
    -- putStrLn "Testing p2 inverts p1"
    -- print $ p2 10007 1 is' 8191
    -- let exs = map fromIntegral $ take 10 $ iterate (p1 10007 is) 2019 -- [2019,8191,8839,7143,1822,6977,8063,4850,6588,5869]
    -- print exs 
    -- print $ zipWith (\i n -> p2 10007 n is' i) exs [0..]
    
    print $ p2 119315717514047 101741582076661 is' 2020
    
    
p1 :: Int -> [Action] -> Int -> Int
p1 l acts i= foldl' go i acts
    where go !ix act = takeActionI l act ix

p2 :: Integer -> Integer -> [Action' Integer] -> Integer -> Integer
p2 l n acts i = 
    let once = reverseMCs l acts 
        multiply a b = reduceMod l a `compose` reduceMod l b
        ntimes = applyMany multiply (ModComp 1 0) once n
     in compute l ntimes i 
    
type Action = Action' Int
data Action' a = 
     DealNewStack
   | DealWithInc  a
   | Cut          a
   deriving (Show, Eq, Ord, Functor)

takeActionI :: Int -> Action -> Int -> Int
takeActionI l act = case act of
    DealNewStack  -> (pred l-)
    Cut         n -> \i -> (i-n) `mod` l
    DealWithInc n -> \i -> (i*n) `mod` l
    
-- (ModComp m b) represents m*x+b
data ModComp = ModComp {multer :: !Integer, adder :: !Integer} deriving (Eq,Show,Ord)

reduceMod :: Integer -> ModComp -> ModComp
reduceMod l (ModComp m b) = ModComp (m `mod` l) (b `mod` l)

compute :: Integer -> ModComp -> Integer -> Integer
compute l (ModComp m b) x = (m * x + b) `mod` l

-- compose (ModComp m b) (ModComp l c) means m*(l*x+c)+b
compose (ModComp m b) (ModComp l c) = ModComp (m*l) (m*c+b)

reverseMC :: Integer -> Action' Integer -> ModComp
reverseMC l act = case act of
    DealNewStack  -> ModComp (-1) (l-1) -- (pred l-) -- (l-1)-x
    Cut         n -> ModComp 1 n -- \i -> (i+n) `mod` l
    DealWithInc n -> ModComp (modInverse n l) 0 -- \i -> (i*modInverse n l) `mod` l 
    
reverseMCs :: Integer -> [Action' Integer] -> ModComp
reverseMCs l = foldr compose (ModComp 1 0) . map (reverseMC l)

-- * Utils
eGCD 0 b = (b, 0, 1)
eGCD a b = let (q,r) = b `divMod` a
               (g, s, t) = eGCD r a
            in (g, t - q * s, s)
           
modInverse a m = let (_,b,_) = eGCD a m in b

toDoubling :: Integer -> [Bool]
toDoubling 0 = []
toDoubling n = odd n : toDoubling (n `div` 2)
    
applyMany :: (a -> a -> a) ->  a -> a -> Integer -> a
applyMany multiply ident x = fst . foldl' go (ident,x) . toDoubling
    where
    go (!a,!c) b = (if b then multiply c a else a,multiply c c)
        
-- * Parsing
getInput :: IO [Action]
getInput = readFile "22.txt" >>= either fail return . getInput'
getInput' :: String -> Either String [Action]
getInput' = parseOnly (parseInput <* skipSpace <* endOfInput) . T.pack

parseLine :: Parser Action
parseLine = pDealNewStack <|> pCut <|> pDealWithInc
    where
    pDealNewStack = DealNewStack <$ "deal into new stack"
    pCut = fmap Cut $ "cut " *> signed decimal
    pDealWithInc  = fmap DealWithInc $ "deal with increment " *> decimal

parseInput :: Parser [Action]
parseInput = sepBy1' parseLine "\n"

-- * Testing
type Deck = V.Vector Int

p1d :: Int -> [Action] -> Deck
p1d l = foldl' (flip takeAction) (V.generate l id)

takeAction :: Action -> Deck -> Deck
takeAction act v = case act of
    DealNewStack -> V.reverse v
    Cut n -> 
        let c = n `mod` V.length v
            (a,b) = V.splitAt c v
         in b <> a
    DealWithInc n -> V.backpermute v (shuffOrd (V.length v) n)
    
shuffOrd :: Int -> Int -> V.Vector Int
shuffOrd l n = V.create $ do
    vec <- VM.new l
    forM_ [0..l-1] $ \i -> let t = (i*n) `mod` l in VM.write vec t i
    pure vec
    
reverseAction :: Integral a => a -> Action' a -> a -> a 
reverseAction l act = case act of
    DealNewStack  -> (pred l-)
    Cut         n -> \i -> (i+n) `mod` l
    DealWithInc n -> \i -> (i*modInverse n l) `mod` l 
    
-- * ex
ex0,ex1,ex2,ex3 :: [Action]
ex0 = either error id $ getInput' "deal with increment 7\
\\ndeal into new stack\
\\ndeal into new stack"

ex1 = either error id $ getInput' "cut 6\
\\ndeal with increment 7\
\\ndeal into new stack"

ex2 = either error id $ getInput' "deal with increment 7\
\\ndeal with increment 9\
\\ncut -2"

ex3 = either error id $ getInput' "deal into new stack\
\\ncut -2\
\\ndeal with increment 7\
\\ncut 8\
\\ncut -4\
\\ndeal with increment 7\
\\ncut 3\
\\ndeal with increment 9\
\\ndeal with increment 3\
\\ncut -1"