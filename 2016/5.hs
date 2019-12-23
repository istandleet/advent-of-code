{-# language OverloadedStrings #-}
module Main where 

import Control.Applicative
import Control.Lens
import qualified Data.List
import qualified Data.ByteString.Lazy.Char8 as CLBS
import Data.Digest.Pure.MD5

import System.Environment


main = do
    putStrLn "5"
    [key] <- getArgs
    -- putStrLn $ part1 $ CLBS.pack key
    putStrLn $ part2 $ CLBS.pack key
    
part1 :: CLBS.ByteString -> String
part1 key = take 8
    [ md !! 5
    | i <- [1..]
    , let md = show $ md5 $ key <> (CLBS.pack $ show i)
    , "00000" `Data.List.isPrefixOf` md
    ]
    
part2 :: CLBS.ByteString -> String
part2 key = go (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)
    [ (md !! 5,md!!6)
    | i <- [1..]
    , let md = show $ md5 $ key <> (CLBS.pack $ show i)
    , "00000" `Data.List.isPrefixOf` md
    , md!!5 < '8'
    ]
    where
    go (Just c1,Just c2,Just c3,Just c4,Just c5,Just c6, Just c7,Just c8) _
        = [c1,c2,c3,c4,c5,c6,c7,c8]
    go cs ((pos,c):ls) =
       let cs' = cs & l pos %~ (<|> Just c)
        in go cs' ls 
    l '0' = _1
    l '1' = _2
    l '2' = _3
    l '3' = _4
    l '4' = _5
    l '5' = _6
    l '6' = _7
    l '7' = _8