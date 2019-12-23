{-# language OverloadedStrings #-}
module Main where 

import qualified Data.ByteString.Lazy.Char8 as CLBS
import Data.Digest.Pure.MD5


main = do
    putStrLn "4"
    let key = "yzbqklnj"
    print $ part1 key
    print $ part2 key
    
part1 :: CLBS.ByteString -> Int
part1 key = head
    [ i
    | i <- [1..]
    , take 5 (show $ md5 $ key <> (CLBS.pack $ show i)) == "00000"
    ]
part2 :: CLBS.ByteString -> Int
part2 key = head
    [ i
    | i <- [1..]
    , take 6 (show $ md5 $ key <> (CLBS.pack $ show i)) == "000000"
    ]