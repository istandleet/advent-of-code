{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Either
import Data.Foldable
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as P hiding (takeWhile)

main :: IO ()
main = do
    s <- T.pack <$> readFile "04.txt"
    let input = either error id $ P.parseOnly readPassports s
    print $ part1 input
    print $ part2 input

type Passport = Map Text Text

part1 = length . filter hasFields

hasFields :: Passport -> Bool
hasFields = (==requiredFields) . Set.intersection requiredFields . Map.keysSet

requiredFields :: Set Text
requiredFields = Set.fromList 
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    -- , "cid"
    ]

part2 = length . filter validatePP

validatePP :: Passport -> Bool
validatePP pp = hasFields pp
             && matchesParser byr (pp Map.! "byr")
             && matchesParser iyr (pp Map.! "iyr")
             && matchesParser eyr (pp Map.! "eyr")
             && matchesParser hgt (pp Map.! "hgt")
             && matchesParser hcl (pp Map.! "hcl")
             && matchesParser ecl (pp Map.! "ecl")
             && matchesParser pid (pp Map.! "pid")

matchesParser :: Parser a -> Text -> Bool
matchesParser p = isRight . parseOnly p

-- Parsing
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
byr = do
    y <- P.count 4 digit
    endOfInput
    let year = read y
    guard $ year >= 1920 && year <= 2002
    
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
iyr = do
    y <- P.count 4 digit
    endOfInput
    let year = read y :: Int
    guard $ year >= 2010 && year <= 2020

-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
eyr = do
    y <- P.count 4 digit
    endOfInput
    let year = read y :: Int
    guard $ year >= 2020 && year <= 2030

{-
hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
-}
hgt = do
    ht <- decimal :: Parser Int
    s <- "cm" <|> "in"
    endOfInput
    guard $ (s == "cm" && ht >= 150 && ht <= 193)
         || (s == "in" && ht >=  59 && ht <=  76)

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
hcl = do
    char '#'
    clr <- P.count 6 $ P.satisfy isHexDigit
    endOfInput

-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
ecl = do
    choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    endOfInput

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
pid = do
    P.count 9 digit
    endOfInput

readPassports :: Parser [Passport]
readPassports = (readPassport `P.sepBy1` (eol <* eol)) <* eol <* P.endOfInput
    where eol = P.endOfLine

readPassport :: Parser Passport
readPassport = fmap Map.fromList $ readField `P.sepBy1` P.skip endOfField

readField :: Parser (Text,Text)
readField = do
    field <- P.take 3
    _ <- P.char ':'
    value <- P.takeWhile1 (not . endOfField)
    return (field, value)

endOfField c = c == '\n' || c == ' '
