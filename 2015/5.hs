{-# language OverloadedStrings #-}
module Main where 

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List

main = do
    putStrLn "5"
    s <- T.lines . T.pack <$> readFile "5.txt"
    print $ part1 s
    print $ part2 s

    
part1 :: [Text] -> Int
part1 = length . filter nicestring1

nicestring1 :: Text -> Bool
nicestring1 s =
       T.length (T.filter isvowel s) >= 3
    && hasDouble s
    && not (any (`T.isInfixOf` s) ["ab", "cd", "pq", "xy"])
    where
    isvowel :: Char -> Bool
    isvowel c = c `elem` ("aeiou" :: String)
    
hasDouble :: Text -> Bool
hasDouble t = or $ map (uncurry (==)) $ T.zip t (T.tail t)


part2 :: [Text] -> Int
part2 = length . filter (nicestring2 . T.unpack)
  
nicestring2 :: String -> Bool
nicestring2 = go False False
    where
    go True True _ = True 
    go _ _ [] = False
    go _ _ [_] = False
    go _ _ [_,_] = False
    go doublePair False [a,_,c] = doublePair && a == c
    go doublePair aba (a:b:c:cs) = 
        let dp' = doublePair || [a,b] `Data.List.isInfixOf` (c:cs)
            aba' = aba || a == c
         in go dp' aba' (b:c:cs)