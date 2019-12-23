{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language TupleSections #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Tuple (swap)
import Data.Maybe

import Data.Attoparsec.Text hiding (D,take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T

main = do
    input <- getInput
    if not (validateInput input) then fail "bad assertion" else return()
    let formula = fromInput input
    print $ p1 formula
    print $ p2 formula
    
p1 :: Formulae -> Int
p1 = oreReq 1


p2 :: Formulae -> Int
p2 f = binSearch ((<1000000000000) . (`oreReq` f))
    

     
oreReq :: Int -> Formulae -> Int
oreReq num m = (\(Made n _) -> n) $ (Map.! "ORE") $ last $ process num m
     
process :: Int -> Formulae -> [State]
process num m = go $ Map.singleton "FUEL" (Requested num)
    where go s = s : if null (getNeeds s) then [] else go $ step m s
          
    
requirements :: Formulae -> String -> Int -> (FuelType,Map String FuelType)
requirements _ "ORE" num = (Made 0 num,mempty)
requirements m s num =
    let (i,f) = m Map.! s
        (q,r) = num `divMod` i
        q' = if r > 0 then q+1 else q
     in (Made 0 (i*q')
        ,fmap Requested $ (q' *) <$> f
        )
    
data FuelType = Requested Int
              | Made Int Int     -- used remain
              | NeedsMore Int Int -- have need
              deriving (Show,Eq)
instance Semigroup FuelType where
    Requested i <> Requested i' = Requested (i+i')
    NeedsMore i j <> NeedsMore i' j' = NeedsMore (i+i') (j+j')
    Made i j <> Made i' j' = Made (i+i') (j+j')
    Requested i <> Made u j 
        | j >= i = Made (u+i) (j-i)
        | otherwise = NeedsMore (u+j) (i-j)
    Requested i <> NeedsMore i' j = NeedsMore i' (i+j)
    Made i j <> NeedsMore i' j'
        | j >= j' = Made (i+i'+j') (j-j')
        | otherwise = NeedsMore (i+i'+j) (j'-j)
    a <> b = b <> a
    
hasNeed (Requested i) = Just i
hasNeed (NeedsMore _ i) = Just i
hasNeed _ = Nothing

type State = Map String FuelType
step :: Formulae -> State -> State
step f s = 
    let needs = getNeeds s
        ts = map (\(k,num) -> let (ft,stuff) = requirements f k num in (Map.singleton k ft,stuff)) $ Map.toList needs
        mades = foldMap fst ts
        newreq = Map.unionsWith (<>) $ map snd ts
     in Map.unionsWith (<>) [s,mades,newreq]

getNeeds :: State -> Map String Int
getNeeds s = Map.fromListWith (+) $
    let mats = Map.keys s in mapMaybe sequence $ zip mats $ map (\k -> hasNeed $ s Map.! k) mats

type Formulae = Map String (Int,Map String Int)
fromInput :: [Line] -> Formulae
fromInput ls = Map.fromList
    [(os,(oi,Map.fromList $ map swap is))
    |(is,(oi,os)) <- ls]

-- * Utils
binSearch :: (Int -> Bool) -> Int
binSearch f = 
    let (passing,failing) = span f $ map (2^) [1..]
     in go (last passing,head failing)
    where
    go (a,b) | a >= b-1 = a
    go (a,b) = go $ let m = (b+a) `div` 2 in if f m then (m,b) else (a,m)
    
-- * Parsing
validateInput :: [Line] -> Bool
validateInput ls =
       length ls == length (Set.fromList $ map snd ls)
    && all (\(is,_) -> length is == length (Set.fromList $ map snd is)) ls

getInput :: IO [Line]
getInput = readFile "14.txt"
       >>= either fail return . parseOnly (parseInput <* skipSpace <* endOfInput) . T.pack

type IS = (Int,String)
type Line = ([IS],IS)

parseIS :: Parser IS
parseIS = do
    i <- decimal 
    char ' '
    s <- many1' letter
    return (i,s)

parseLine :: Parser Line
parseLine = do
    ins <- sepBy1' parseIS ", "
    " => "
    o <- parseIS
    return (ins,o)

parseInput :: Parser [Line]
parseInput = sepBy1' parseLine "\n"


mkF = either error fromInput . parseOnly (parseInput <* skipSpace <* endOfInput) . T.pack
ex0, ex1, ex2, ex3 :: Formulae
ex0 = mkF "9 ORE => 2 A\
\\n8 ORE => 3 B\
\\n7 ORE => 5 C\
\\n3 A, 4 B => 1 AB\
\\n5 B, 7 C => 1 BC\
\\n4 C, 1 A => 1 CA\
\\n2 AB, 3 BC, 4 CA => 1 FUEL"
ex1 = mkF "157 ORE => 5 NZVS\
\\n165 ORE => 6 DCFZ\
\\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\
\\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\
\\n179 ORE => 7 PSHF\
\\n177 ORE => 5 HKGWZ\
\\n7 DCFZ, 7 PSHF => 2 XJWVT\
\\n165 ORE => 2 GPVTF\
\\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
ex2 = mkF "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\
\\n17 NVRVD, 3 JNWZP => 8 VPVL\
\\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\
\\n22 VJHF, 37 MNCFX => 5 FWMGM\
\\n139 ORE => 4 NVRVD\
\\n144 ORE => 7 JNWZP\
\\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\
\\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\
\\n145 ORE => 6 MNCFX\
\\n1 NVRVD => 8 CXFTF\
\\n1 VJHF, 6 MNCFX => 4 RFSQX\
\\n176 ORE => 6 VJHF"
ex3 = mkF "171 ORE => 8 CNZTR\
\\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\
\\n114 ORE => 4 BHXH\
\\n14 VRPVC => 6 BMBT\
\\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\
\\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\
\\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\
\\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\
\\n5 BMBT => 4 WPTQ\
\\n189 ORE => 9 KTJDG\
\\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\
\\n12 VRPVC, 27 CNZTR => 2 XDBXC\
\\n15 KTJDG, 12 BHXH => 5 XCVML\
\\n3 BHXH, 2 VRPVC => 7 MZWV\
\\n121 ORE => 7 VRPVC\
\\n7 XCVML => 6 RJRHP\
\\n5 BHXH, 4 VRPVC => 5 LTCX"