{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Applicative
-- import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile, take, count, Done)

import Data.Time

main = do
    putStrLn "22"
    input <- getInput "22.txt"
    getCurrentTime >>= print
    print $ part1 input
    getCurrentTime >>= print
    print $ part2 input
    getCurrentTime >>= print

part1 :: Input -> Int
part1 = sum . map volume . restrict . go . dropWhile (not . fst)
    where 
    go ((True,c):cs) = apply c cs
    restrict = foldMap (intersect area)
    area = Cuboid (-50,-50,-50) (51,51,51)
part2 = sum . map volume . go . dropWhile (not . fst)
    where go ((True,c):cs) = apply c cs

apply :: Cuboid -> [Action] -> [Cuboid]
apply c = foldl (uncurry . step) [c]

step :: [Cuboid] -> Bool -> Cuboid -> [Cuboid]
step acc False c = foldMap (`without` c) acc
step acc True c = acc <> foldl go [c] acc 
    where go acc' c' = foldMap (`without` c') acc'

type Input = [Action]
type Coord3 = (Int, Int, Int)
data Cuboid = Cuboid Coord3 Coord3 deriving (Show, Eq)
type Action = (Bool, Cuboid)

volume (Cuboid (xmin,ymin,zmin) (xmax,ymax,zmax)) = (xmax-xmin)*(ymax-ymin)*(zmax-zmin)
contents (Cuboid (xmin,ymin,zmin) (xmax,ymax,zmax)) = [(x,y,z)| x <- [xmin..xmax-1], y <- [ymin..ymax-1], z <- [zmin..zmax-1]]

overlaps (Cuboid (xmin,ymin,zmin) (xmax,ymax,zmax)) (Cuboid (xmin',ymin',zmin') (xmax',ymax',zmax')) = 
    not $  xmin' >= xmax  
        || ymin' >= ymax  
        || zmin' >= zmax  
        || xmin  >= xmax' 
        || ymin  >= ymax' 
        || zmin  >= zmax'

intersect :: Cuboid -> Cuboid -> [Cuboid]
intersect a b
    | not $ overlaps a b  = []
    | otherwise = 
        let b' = b `without` a
            a' = a `without` b
         in apply a $ map (False,) (b' <> a')

without :: Cuboid -> Cuboid -> [Cuboid]
without start@(Cuboid (xmin,ymin,zmin) (xmax,ymax,zmax)) remove@(Cuboid (xmin',ymin',zmin') (xmax',ymax',zmax'))
    | not $ overlaps start remove  = [start]
    | otherwise = 
        [ c 
        | (x0,x1) <- zip xs (tail xs)
        , (y0,y1) <- zip ys (tail ys)
        , (z0,z1) <- zip zs (tail zs)
        , let c = Cuboid (x0,y0,z0) (x1,y1,z1)
        , not $ overlaps c remove 
        , volume c > 0
        ]
    where 
    xs = [xmin] <> (if ordered xmin xmin' xmax then [xmin'] else []) <> (if ordered xmin xmax' xmax then [xmax'] else []) <> [xmax]
    ys = [ymin] <> (if ordered ymin ymin' ymax then [ymin'] else []) <> (if ordered ymin ymax' ymax then [ymax'] else []) <> [ymax]
    zs = [zmin] <> (if ordered zmin zmin' zmax then [zmin'] else []) <> (if ordered zmin zmax' zmax then [zmax'] else []) <> [zmax]

ordered a b c = a <= b && b <= c

parseAction :: Parser Action
parseAction = do 
    b <- (True <$ "on") <|> (False <$ "off")
    " x="
    xmin <- signed decimal
    ".."
    xmax <- signed decimal
    ",y="
    ymin <- signed decimal
    ".."
    ymax <- signed decimal
    ",z="
    zmin <- signed decimal
    ".."
    zmax <- signed decimal
    return (b, Cuboid (xmin,ymin,zmin) (xmax+1,ymax+1,zmax+1))
        
getInput :: FilePath -> IO Input
getInput fp = readFile fp >>= either fail return . getInput'
getInput' :: String -> Either String Input
getInput' = P.parseOnly (P.sepBy1' parseAction "\n" <* P.skipSpace <* P.endOfInput) . T.pack
