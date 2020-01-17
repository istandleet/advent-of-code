{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where 

import Data.Char

main = do
    ss <- filter (not . null) . lines <$> readFile "8ex.txt"
    print $ p1 ss -- 12
    print $ p2 ss -- 19
    ss <- filter (not . null) . lines <$> readFile "8.txt"
    print $ p1 ss -- 1350
    print $ p2 ss -- 2085
    
p1 :: [String] -> Int
-- p1 = sum . map (\s -> length s - length (read s :: String))
p1 = sum . map (\s -> length s - length (parse $ tail $ init $ s))
p2 = sum . map (\s -> length (encode s) + 2 - length s)


parse ('\\':'\\':ss) = '\\' : parse ss
parse ('\\':'"':ss) = '"' : parse ss
parse ('\\':'x':a:b:ss) | isHexDigit a && isHexDigit b = read ("'\\x" ++ [a,b] ++ "'") : parse ss
parse ('\\':_) = error "Unparsable string"
parse (c:ss) = c: parse ss
parse [] = []

encode [] = []
encode (c:ss) = case c of 
    '"' -> "\\\"" ++ encode ss
    '\\' -> "\\\\" ++ encode ss
    c -> c : encode ss