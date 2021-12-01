-- cat 01.txt | runghc 01.hs
module Main where

main :: IO ()
main = interact mains

mains :: String -> String
mains = unlines . map show . (\is -> [part1 is, part2 is]) . readints

part1 :: [Int] -> Int
part1 is = length $ filter id $ zipWith (>) (tail is) is

part2 :: [Int] -> Int
part2 is = length $ filter id $ zipWith (>) (tail triads) triads
    where triads = zipWith3 (\a b c -> a + b + c) is (tail is) (tail $ tail is)

readints :: String -> [Int]
readints = map read . filter (not.null) . lines
