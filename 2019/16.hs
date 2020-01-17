{-# language BangPatterns #-}
module Main (main) where

import Data.Char
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    s <- filter isDigit <$> readFile "16.txt"
    let is = map digitToInt s
    putStrLn $ p1 is -- 49254779
    -- putStrLn $ p2 ex20 -- 84462026
    putStrLn $ p2 is

p1 :: [Int] -> String
p1 = map intToDigit . take 8 . fft
p2 :: [Int] -> String
p2 ns = 
    let offset = foldl (\acc d -> 10*acc+d) 0 $ take 7 $ ns
     in map intToDigit $ slice offset 8 . fft $ concat $ replicate 10000 ns
 
fft :: [Int] -> [Int]
-- fft = fftL
fft = V.toList . fftU . V.fromList 

_fftL :: [Int] -> [Int]
_fftL = iterateN 100 step 
    where step !lst = zipWith use cycles lst
            where use !pattern _ = lastDigit $ sum $ zipWith (*) pattern lst
        
fftU :: Vector Int -> Vector Int
fftU = iterateN 100 stepV
    where
    stepV :: Vector Int -> Vector Int
    stepV ls = V.generate l gen
        where
        l = V.length ls
        halfway = ((l+1) `div` 2)+1
        sums = V.postscanr (+) 0 ls
        gen i | i >= halfway = lastDigit $ sums V.! i
        gen i = lastDigit $ sumThrough $ genSumIs i l
        
        sumThrough :: [Int] -> Int
        sumThrough = go 0
            where
            go !acc [] = acc
            go !acc [a] = acc + (sums V.! a)
            go !acc [a,b] = acc + (sums V.! a) - (sums V.! b)
            go !acc [a,b,c] = acc + (sums V.! a) - (sums V.! b) - (sums V.! c)
            go !acc (a:b:c:d:ds) = go (acc + (sums V.! a) - (sums V.! b) - (sums V.! c) + (sums V.! d)) ds
        
        genSumIs :: Int -> Int -> [Int]
        genSumIs ix l = let i = succ ix in [i-1,2*i-1..l-1]

base :: [Int]
base = [0, 1, 0, -1]
cycles :: [[Int]]
cycles = map (tail . cycle) $ map (`dupeList` base) [1..]

dupeList :: Int -> [a] -> [a]
dupeList = foldMap . replicate
    
lastDigit :: Int -> Int
lastDigit n = abs n `mod` 10 

iterateN :: Int -> (b -> b) -> b -> b
iterateN n f = foldr (.) id (replicate n f)

slice :: Int -> Int -> [a] -> [a]
slice n o = take o . drop n

ex = map digitToInt "12345678"
ex10 = map digitToInt "80871224585914546619083218645595"
ex11 = map digitToInt "19617804207202209144916044189917"
ex12 = map digitToInt "69317163492948606335995924319873"
ex20 = map digitToInt "03036732577212944063491565474664"

{-
drawBoards i n = go 1 move
    where
    move = adjacency n
    go j !a = print j >> putStrLn (drawBoard a) >> 
        if j == i then return () else putStrLn "" >> go (succ j) nxt
        where nxt = a `mult` move

adjacency n = array ((1,1),(n,n)) 
    [ ((x,y),n)
    | (y,l) <- zip [1..] $ take n cycles
    , (x,n) <- zip [1..] $ take n l
    ]
    
drawBoard :: Show a => Array (Int,Int) a  -> String
drawBoard m = init $ unlines
    [   Data.List.intercalate "\t" [ show (m !(x,y))
        | x<-[x0..x1]
        ]
    | y <- [y0..y1]
    ]
    where
    ((x0,y0),(x1,y1)) = bounds m

mult :: Num a => Array (Int,Int) a -> Array (Int,Int) a -> Array (Int,Int) a
mult a b = array ((bx0,ay0),(bx1,ay1)) 
    [ ((bx,ay), sum [ a ! (ax0+i,ay) * b ! (bx,by0+i) 
                    | i <- [0..ax1-ax0]
                    ])
    | ay <- [ay0..ay1]
    , bx <- [bx0..bx1]
    ]
    where
    ((ax0,ay0),(ax1,ay1)) = bounds a
    ((bx0,by0),(bx1,by1)) = bounds b
-}