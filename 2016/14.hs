{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
module Main where 

import Control.Applicative
import Control.Lens
import Data.Array
import Data.Maybe
import qualified Data.List
import qualified Data.ByteString.Lazy.Char8 as CLBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Data.Digest.Pure.MD5
import Control.Parallel

import System.Environment


main = do
    [key] <- getArgs
    putStrLn "14"
    -- print $ part1 key
    mapM_ print $ take 64 $ keys2 key
    
part1 :: String -> Int
part1 key = keys key !! 63

keys :: String -> [Int]
keys key = go $ zip [0..] $ hashes key
    where
    go ((i,k):ks) = case getTriplet k of
        Nothing -> go ks
        Just c -> if any (isKeyMatch c . snd) $ take 1000 ks
                    then i: go ks else go ks
    
hashes key = map (T.pack . md5s . (key ++) . show) [0..]

part2 :: String -> Int
part2 key = keys2 key !! 63

keys2 :: String -> [Int]
keys2 key = fstchunk `par` sndchunk `par` go 0 fstchunk sndchunk hashlist''
    where
    hashlist = hashes2 key
    fstchunk = Vector.fromList as
    sndchunk = Vector.fromList bs
    (as,hashlist')  = splitAt 2000 hashlist
    (bs,hashlist'') = splitAt 2000 hashlist'
    go !i a b c 
        | length a < 1200 = 
            let (newb,c') = splitAt 2000 c
                b' = Vector.fromList newb
             in b' `par` go i (Vector.force $ a <> b) b' c'
        | otherwise = 
            let k = Vector.head a
                a' = Vector.tail a
                rest = go (succ i) a' b c
             in case getTriplet k of
                    Nothing -> rest
                    Just char -> if any (isKeyMatch char) $ Vector.take 1000 a'
                                then i: rest else rest
    
hashes2 :: String -> [Text]
hashes2 key = map (T.pack . hashing 2017 . (key ++) . show) [0..]
    where
    hashing n s = iterate md5s s !! n
    
md5s :: String -> String
md5s = show . md5 . CLBS.pack
    
getTriplet :: Text -> Maybe Char
getTriplet ts = listToMaybe
    [ a
    | (a,(b,c)) <- zip s (zip (tail s) (drop 2 s))
    , a == b
    , b == c
    ]
    where s = T.unpack ts
     
isKeyMatch :: Char -> Text -> Bool
isKeyMatch c s = T.pack (replicate 5 c) `T.isInfixOf` s