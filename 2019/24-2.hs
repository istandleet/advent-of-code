{-# language BangPatterns #-}
{-# language TupleSections #-} 
{-# language FlexibleContexts #-}
module Main where

import Data.Foldable
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Array.Unboxed

main :: IO ()
main = do
    print $ countBugs $ iterate step (Seq.singleton ex) !! 10
    print $ countBugs $ iterate step (Seq.singleton input) !! 200

countBugs :: Board -> Int
countBugs = sum . fmap (length . filter id . elems) 

-- * Robot
type Coord = (Int,Int)
type Layer = UArray Coord Bool
type Board = Seq Layer

deadLayer :: Layer
deadLayer = listArray ((1,1),(5,5)) $ repeat False
           
step :: Board -> Board
step b = stripDeadEnds $ Seq.zipWith go b' (neighborCount b)
    where
    b' = deadLayer :<| (b :|> deadLayer)
    
    go :: Layer -> UArray Coord Int -> Layer
    go l is = listArray bnds $ map f (range bnds)
        where
        f (3,3) = False
        f ix = near == 1 || (near == 2 && not (l ! ix))
            where near = is ! ix
    bnds = ((1,1),(5,5))
    
neighborCount :: Board -> Seq (UArray Coord Int)
neighborCount b = Seq.zipWith3 neighboring 
    (deadLayer :<| deadLayer :<| b) 
    (deadLayer :<| (b :|> deadLayer))
    (b :|> deadLayer :|> deadLayer)
    
-- putStrLn $ drawLayer' (head . show) $ neighboring deadLayer deadLayer ex
neighboring :: Layer -> Layer -> Layer -> UArray Coord Int
neighboring t m b = listArray bnds $ map f (range bnds)
    where
    f (3,3) = 0
    f ix = length (filter (t !) tc)
         + length (filter (m !) mc)
         + length (filter (b !) bc)
        where (tc,mc,bc) = neighbors ix
    bnds = ((1,1),(5,5))
    
neighbors :: Coord -> ([Coord],[Coord],[Coord])
neighbors c@(x,y) = (xups++yups,mids,bots)
    where
    mids = neighbors' c
    
    xups = case fst c of 
        1 -> [(2,3)] 
        5 -> [(4,3)] 
        _ -> []
    yups = case snd c of
        1 -> [(3,2)] 
        5 -> [(3,4)] 
        _ -> []
    
    bots = case c of 
        (2,3) -> map (1,) [1..5]
        (4,3) -> map (5,) [1..5]
        (3,2) -> map (,1) [1..5]
        (3,4) -> map (,5) [1..5]
        _ -> []

neighbors' :: Coord -> [Coord]
neighbors' (x,y) = filter (\c -> inRange ((1,1),(5,5)) c && c/=(3,3))
    [(x-1,y)
    ,(x+1,y)
    ,(x,y-1)
    ,(x,y+1)
    ]
    
stripDeadEnds :: Board -> Board
-- stripDeadEnds = Seq.dropWhileL (==deadLayer) . Seq.dropWhileR (==deadLayer)
stripDeadEnds (a :<| (b :|> c)) | a == deadLayer && c == deadLayer = stripDeadEnds b
stripDeadEnds b = b

-- ** Strings
readLayer :: String -> Layer
readLayer s = array ((1,1),(5,5))
    [ ((x,y),(x,y)/=(3,3) && c=='#')
    | (y,l) <- zip [1..5] $ lines s
    , (x,c) <- zip [1..5] $ l
    ]

drawBoard :: Board  -> String
drawBoard = drawBoard' (\b -> if b then '#' else '.')

drawBoardN :: Board  -> String
drawBoardN = drawBoard' (head . show) . neighborCount

drawLayer' :: IArray UArray a => (a -> Char) -> UArray Coord a  -> String
drawLayer' f m = init $ unlines 
    [   [ if (x,y) == (3,3) then '?' else f $ m !(x,y)
        |x<-[1..5]
        , let c = (x,y)
        ]
    | y <- [1..5]
    ]
    
drawBoard' :: IArray UArray a => (a -> Char) -> Seq (UArray Coord a)  -> String
drawBoard' f b = 
    let l = length b
        m = l `div` 2
     in unlines $ toList $ Seq.zipWith go (Seq.fromList $ map (subtract m) [0..l]) b
    where
    go i layer = unlines ["Depth: " ++ show i ,drawLayer' f layer]
    
-- * Utils
ex = readLayer "....#\
\\n#..#.\
\\n#..##\
\\n..#..\
\\n#...."

input = readLayer "#..##\
\\n#.#..\
\\n#...#\
\\n##..#\
\\n#..##"