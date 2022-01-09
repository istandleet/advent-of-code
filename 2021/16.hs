{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
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

main :: IO ()
main = do
    print $ part1 input
    print $ part2 input

part1 :: String -> Int
part1 = go . readPacket
    where
    go Literal{..} = version
    go Operation{..} = version + sum (map go packets)

part2 :: String -> Integer
part2 = run . readPacket
    
run Literal{..} = number
run Operation{..} = f (map run packets)
    where 
    f = case typeId of 
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> \[a,b] -> if a > b then 1 else 0
        6 -> \[a,b] -> if a < b then 1 else 0
        7 -> \[a,b] -> if a == b then 1 else 0

input = "805311100469800804A3E488ACC0B10055D8009548874F65665AD42F60073E7338E7E5C538D820114AEA1A19927797976F8F43CD7354D66747B3005B401397C6CBA2FCEEE7AACDECC017938B3F802E000854488F70FC401F8BD09E199005B3600BCBFEEE12FFBB84FC8466B515E92B79B1003C797AEBAF53917E99FF2E953D0D284359CA0CB80193D12B3005B4017968D77EB224B46BBF591E7BEBD2FA00100622B4ED64773D0CF7816600B68020000874718E715C0010D8AF1E61CC946FB99FC2C20098275EBC0109FA14CAEDC20EB8033389531AAB14C72162492DE33AE0118012C05EEB801C0054F880102007A01192C040E100ED20035DA8018402BE20099A0020CB801AE0049801E800DD10021E4002DC7D30046C0160004323E42C8EA200DC5A87D06250C50015097FB2CFC93A101006F532EB600849634912799EF7BF609270D0802B59876F004246941091A5040402C9BD4DF654967BFDE4A6432769CED4EC3C4F04C000A895B8E98013246A6016CB3CCC94C9144A03CFAB9002033E7B24A24016DD802933AFAE48EAA3335A632013BC401D8850863A8803D1C61447A00042E3647B83F313674009E6533E158C3351F94C9902803D35C869865D564690103004E74CB001F39BEFFAAD37DFF558C012D005A5A9E851D25F76DD88A5F4BC600ACB6E1322B004E5FE1F2FF0E3005EC017969EB7AE4D1A53D07B918C0B1802F088B2C810326215CCBB6BC140C0149EE87780233E0D298C33B008C52763C9C94BF8DC886504E1ECD4E75C7E4EA00284180371362C44320043E2EC258F24008747785D10C001039F80644F201217401500043A2244B8D200085C3F8690BA78F08018394079A7A996D200806647A49E249C675C0802609D66B004658BA7F1562500366279CCBEB2600ACCA6D802C00085C658BD1DC401A8EB136100"

exampleLit = "D2FE28"

readPacket :: String -> Packet
readPacket = evalState parsePacket . readHex
readPackets [] = []
readPackets s = let (a,s') = runState parsePacket s in a : readPackets s'

data Packet
   = Literal 
   { version :: Int 
   , typeId  :: Int
   , number  :: Integer
   } 
   | Operation 
   { version :: Int 
   , typeId  :: Int
   , packets :: [Packet]
   } 
   deriving (Show, Eq)

type Parser = State [Bool]

parsePacket :: Parser Packet
parsePacket = do 
    version <- parseLength 3
    tb <- state $ splitAt 3
    let typeId = fromBits tb 
    case typeId of 
        4 -> Literal   version typeId <$> parseLiteral
        _ -> Operation version typeId <$> parseOperation

parseLiteral :: Parser Integer
parseLiteral = fromBits <$> do
    a <- state $ splitAt 5
    fmap (tail a ++) $
        if head a then go else pure []

parseOperation :: Parser [Packet]
parseOperation = do
    lengthId <- state $ \(c:cs) -> (c,cs)
    case lengthId of 
        False -> parseFixed
        True -> parseReplicate
    where 
    parseFixed = do 
        l <- parseLength 15
        b <- state $ splitAt l
        pure $ readPackets b
    parseReplicate = do 
        l <- parseLength 11
        replicateM l parsePacket

parseLength :: Int -> Parser Int
parseLength = fmap fromBits . state . splitAt

fromBits :: (Bits a, Integral a) => [Bool] -> a
fromBits = foldr (flip setBit) 0 . map fst . filter snd . zip [0..] . reverse


readHex :: String -> [Bool]
readHex = foldMap toBits
toBits :: Char -> [Bool]
toBits = map odd . go
  where 
  go = \case
    '0' -> [0,0,0,0]
    '1' -> [0,0,0,1]
    '2' -> [0,0,1,0]
    '3' -> [0,0,1,1]
    '4' -> [0,1,0,0]
    '5' -> [0,1,0,1]
    '6' -> [0,1,1,0]
    '7' -> [0,1,1,1]
    '8' -> [1,0,0,0]
    '9' -> [1,0,0,1]
    'A' -> [1,0,1,0]
    'B' -> [1,0,1,1]
    'C' -> [1,1,0,0]
    'D' -> [1,1,0,1]
    'E' -> [1,1,1,0]
    'F' -> [1,1,1,1]
