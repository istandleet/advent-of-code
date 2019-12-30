{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NoMonomorphismRestriction #-}
-- {-# language TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List
import System.IO

import Data.Attoparsec.Text hiding (D,take, Result)
import Data.Text (Text)
import qualified Data.Text as T

import Intcode

-- * Main
main :: IO ()
main = do
    dat <- read . (\s -> "["++s++"]") <$> readFile "25.txt" :: IO Program
    
    -- -- Interactive Mode:
    -- hSetBuffering stdout NoBuffering
    -- hSetBuffering stdin  LineBuffering
    -- let (s,comp) = runState (interactAscii "") $ initComputer $ dat
    -- putStrLn s
    -- interactive comp
    
    -- Watch Mode:
    -- bot <- watchBot dat
    -- mapM_ print $ findInterestItemSets bot
    
    -- Pure Mode:
    print $ p1 dat -- 2236672 

p1 :: Program -> Maybe Int
p1 = findPassword . gatherItemsPure . startProgram 
    
-- ** IO
interactive :: Computer -> IO ()
interactive c = go ("Command?",c)
    where
    go (s,comp) = do
        putStrLn s
        cmd <- getLine
        case cmd of 
            ":q" -> return ()
            _ -> go $ runState (interactAscii $ cmd ++ "\n") comp
            
watchBot :: Program -> IO Bot
watchBot = start . runState (interactAscii "") . initComputer
    where
    start (s,comp) = do
        putStrLn s
        out <- either fail pure $ readOutput s
        print out
        let bot = updateWithOut (0,0) out $ Bot (0,0) mempty mempty comp
        watchGatherItems bot
        
watchGatherItems :: Bot -> IO Bot
watchGatherItems bot = do
    print (bot ^. position)
    print (bot ^. inventory)
    print $ fmap name (bot ^. outlook)
    case gatherItemTurn bot of 
        Nothing -> pure bot
        Just ((s,out,command,target),bot) -> do
            print (command,target)
            putStrLn s
            watchGatherItems bot
        
-- ** Pure
-- dat <- read . (\s -> "["++s++"]") <$> readFile "25.txt" :: IO Program
-- let bot = gatherItemsPure $ startProgram dat
-- interactive $ _computer bot
startProgram :: Program -> Bot
startProgram = start . runState (interactAscii "") . initComputer
    where
    start (s,comp) = 
        let out = either (\e -> error $ unlines ["Unrecognized Output",show s,e]) id $ readOutput s
         in updateWithOut (0,0) out $ Bot (0,0) mempty mempty comp
         
gatherItemsPure :: Bot -> Bot
gatherItemsPure b = maybe b (gatherItemsPure . snd) $ gatherItemTurn b 

type Turn = (String,Output,Command,Coord)
gatherItemTurn :: Bot -> Maybe (Turn,Bot)
gatherItemTurn bot = case chooseGatheringMove bot of
    Nothing -> Nothing
    Just command -> Just $ 
        let target = case command of 
                Move d -> move d (bot ^. position)
                _ -> bot ^. position
            (s,comp) = runState (interactAscii $ comString command) (bot ^. computer)
            out = either (\e -> error $ unlines ["Unrecognized Output",show s,e]) id $ readOutput s
            r = (s,out,command,target)
            bot' = updateWithOut target out $ computer .~ comp $ bot
         in (r,bot')

badItems :: Set Item
badItems = Set.fromList ["escape pod","giant electromagnet","photons","molten lava","infinite loop"]
psfRoom :: Room
psfRoom = Room "Pressure-Sensitive Floor" "Analyzing..." [L] mempty

findPassword :: Bot -> Maybe Int
findPassword bot = go bot $ Set.toList $ bot ^. inventory
    where 
    go bot [] = test bot
    go bot (i:is) = go bot is <|> go (dropItem bot i) is
    test bot = case readOutput s of
        Right (RoomMove _ (Password p)) -> Just p
        Right (RoomMove _ (Rejection _)) -> Nothing
        Left e -> error $ unlines ["Unparsed output",s,e]
        _ -> Nothing
        where s = evalState (interactAscii $ comString $ Move R) (bot ^. computer)
    
findInterestItemSets :: Bot -> [(String,Set Item)]
findInterestItemSets original = go original $ Set.toList $ original ^. inventory
    where 
    go bot [] = test bot
    go bot (i:is) = go bot is <> go (dropItem bot i) is
    test bot = maybeToList $ case readOutput s of
        Right (RoomMove _ (Rejection _)) -> Nothing
        Right out -> Just (unlines["Unexpected output",show s,show out], bot ^. inventory)
        Left _ -> Just (unlines ["New output",s], bot ^. inventory)
        where s = evalState (interactAscii $ comString $ Move R) (bot ^. computer)

runDropItemSet :: Bot -> Set Item -> String
runDropItemSet bot = test . foldl dropItem bot 
    where test bot = evalState (interactAscii $ comString $ Move R) (bot ^. computer)

dropItem :: Bot -> Item -> Bot
dropItem bot i = 
    let command = Drop i
        target = bot ^. position
        (s,comp) = runState (interactAscii $ comString command) (bot ^. computer)
        out = either (\e -> error $ unlines ["Unrecognized Output",show s,e]) id $ readOutput s
     in case out of 
            Dropped i' | i == i' -> updateWithOut target out $ computer .~ comp $ bot
            _ -> error $ unlines ["Unexpected output when dropping item " ++ show i,show s,show out]

-- * Algo
data Bot = Bot
   { _position  :: !Coord
   , _outlook   :: !(Map Coord Room)
   , _inventory :: !(Set Item)
   , _computer  :: !Computer
   } deriving (Show,Eq)
   
chooseGatheringMove :: Bot -> Maybe Command
chooseGatheringMove bot 
    | not (null goodItems) = Just $ Take $ Set.findMin goodItems
    | not (null unexplored) = Just $ Move $ fst $ head $ pathTo dirmap (_position bot) (Set.findMin unexplored)
    | name currRoom == "Security Checkpoint" = Nothing
    | otherwise = fmap (Move . fst . head . pathTo dirmap (_position bot)) secPointPos 
    where
    currRoom = case Map.lookup (_position bot) (_outlook bot) of
        Just r -> r
        Nothing -> error $ show (_position bot) ++ " is not a known room. Rooms: " ++ show (_outlook bot)
    goodItems = items currRoom Set.\\ badItems
    dirmap = fmap exits $ _outlook bot
    explored = Map.keysSet dirmap
    unexplored = discoverable dirmap Set.\\ explored
    secPointPos = fmap fst $ Data.List.find ((== "Security Checkpoint") . name . snd) $ Map.toList $ _outlook bot 

updateWithOut :: Coord -> Output -> Bot -> Bot
updateWithOut target out = case out of 
    RoomMove r CommandPrompt -> (position .~ target) . (outlook %~ Map.insert target r)
    RoomMove _ (Rejection _) -> outlook %~ Map.insert target psfRoom
    RoomMove _ (Password p) -> error $ unwords ["Found password:",show p]
    PickedUp i -> (inventory %~ (Set.insert i)) . (outlook %~ Map.adjust (lItems %~ removeItem i) target)
    Dropped  i -> (inventory %~ removeItem i) . (outlook %~ Map.adjust (lItems %~ (Set.insert i)) target)
    Failed -> id

removeItem :: Item -> Set Item -> Set Item
removeItem i is = 
    if i `Set.notMember` is then error $ "Item " ++ show i ++ " not in list: " ++ show is else Set.delete i is

-- * Simulator
data Command = 
     Move Dir
   | Take Item
   | Drop Item
   | ListItems
   deriving (Show,Eq)
comString = \case
    Move d    -> dirString d                 ++ "\n"
    Take i    -> unwords ["take",T.unpack i] ++ "\n"
    Drop i    -> unwords ["drop",T.unpack i] ++ "\n"
    ListItems -> "inv"                       ++ "\n"

type Item = Text
data Output = 
     RoomMove Room Result
   | PickedUp Item
   | Dropped  Item
   | Failed
   deriving (Show,Eq,Ord)
data Room = Room
   { name :: !Text
   , description :: !Text
   , exits :: ![Dir]
   , items :: !(Set Item)
   } deriving (Show,Eq,Ord)
data Result = 
     CommandPrompt
   | Rejection (Bool,Output)
   | Password Int
   deriving (Show,Eq,Ord)
   
-- * Movement
type Coord = (Int,Int)
data Dir = U | L | R | D deriving (Eq, Show, Read, Ord, Enum, Bounded)
move :: Dir -> Coord -> Coord
move = \case
    U -> _2 %~ succ
    L -> _1 %~ pred
    R -> _1 %~ succ
    D -> _2 %~ pred
oppositeDir :: Dir -> Dir
oppositeDir = \case
    U -> D
    L -> R
    R -> L
    D -> U
dirString :: Dir -> String
dirString = \case
    U -> "north"
    L -> "west"
    R -> "east"
    D -> "south"

pathTo :: Map Coord [Dir] -> Coord -> Coord -> [(Dir,Coord)]
pathTo _ a b | a == b = []
pathTo s a b = reverse $ 
    let neighbors = map (\d -> (d,move d a)) (s Map.! a)
     in case find ((==b) . snd) neighbors of
                Just p -> [p]
                Nothing -> go (Set.fromList $ map snd neighbors) $ map pure neighbors 
    where
    go !seen paths = 
        let paths' = 
              [ (d,c'):path
              | path <- paths
              , let (_,c) = head path
              , d <- Map.findWithDefault [] c s
              , let c' = move d c
              , c' `Set.notMember` seen
              ]
            seen' = seen <> Set.fromList (map (snd . head) paths')
         in case find ((==b) . snd . head) paths' of
                Just p -> p
                Nothing -> if null paths' 
                            then error $ "No path found between " ++ show a ++ " and " ++ show b ++ " in " ++ show s
                            else go seen' paths'

discoverable :: Map Coord [Dir] -> Set Coord
discoverable = Map.foldMapWithKey $ \k ds -> Set.insert k $ Set.fromList $ map (`move` k) ds

-- * Parsing
readOutput :: String -> Either String Output
readOutput = parseOnly parseOutput . T.pack

-- ** Output
parseOutput :: Parser Output
parseOutput = ( parseRoomMove <?> "RoomMove" )
          <|> ( parsePickedUp <?> "PickedUp" )
          <|> ( parseDropped  <?> "Dropped"  )
          <|> ( parseFailed   <?> "Failed"   )
parsePickedUp :: Parser Output
parsePickedUp = do
    skipSpace
    "You take the " 
    item <- takeWhile1 ('.'/=)
    char '.'
    skipSpace
    parseCommand
    skipSpace
    endOfInput
    pure $ PickedUp item
parseDropped :: Parser Output
parseDropped = do
    skipSpace
    "You drop the " 
    item <- takeWhile1 ('.'/=)
    char '.'
    skipSpace
    parseCommand
    skipSpace
    endOfInput
    pure $ Dropped item
parseFailed :: Parser Output
parseFailed = do
    skipSpace
    "You don't see that item here."  <|> "You can't go that way." <|> "Unrecognized command." <|> "You don't have that item."
    skipSpace
    parseCommand
    skipSpace
    endOfInput
    pure $ Failed
parseRoomMove :: Parser Output
parseRoomMove = do
    skipSpace
    room <- parseRoom <?> "Room"
    skipSpace
    result <- parseResult <?> "Result"
    skipSpace
    endOfInput
    pure $ RoomMove room result

-- ** Result
parseResult :: Parser Result
parseResult = parseCommand <|> parseRejection <|> parsePassword
parseCommand :: Parser Result
parseCommand = CommandPrompt <$ "Command?" 
parseRejection :: Parser Result
parseRejection = do
    "A loud, robotic voice says \"Alert! Droids on this ship are "
    b <- (True <$ "heavier") <|> (False <$ "lighter")
    " than the detected value!\" and you are ejected back to the checkpoint."
    o <- parseOutput
    pure $ Rejection (b,o)
parsePassword :: Parser Result
parsePassword = do
    "A loud, robotic voice says \"Analysis complete! You may proceed.\" and you enter the cockpit."
    endOfLine
    "Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly."
    endOfLine
    "\"Oh, hello! You should be able to get in by typing "
    p <- decimal
    " on the keypad at the main airlock.\""
    pure $ Password p
    

-- ** Room
parseRoom = do
    name <- parseRoomName <?> "Room Name"
    endOfLine
    description <- parseRoomDescription <?> "Room Description"
    endOfLine
    exits <- parseExits <?> "Exits"
    endOfLine
    items <- option mempty (parseItems <* endOfLine) <?> "Items"
    return Room{..}

parseRoomName :: Parser Text
parseRoomName = fmap T.unwords $ "== " *> sepBy1' pword (char ' ') <* " =="

parseRoomDescription :: Parser Text
parseRoomDescription = takeWhile1 (not . isEndOfLine) <* endOfLine

parseExits :: Parser [Dir]
parseExits = do
    "Doors here lead:" <?> "Header"
    endOfLine
    o <- sepBy1' ("- " *> parseDir) endOfLine <?> "List"
    endOfLine
    pure o
    
parseItems :: Parser (Set Item)
parseItems = Set.fromList <$> do
    "Items here:" <?> "Header"
    endOfLine
    o <- sepBy1' ("- " *> takeWhile1 (not . isEndOfLine)) endOfLine <?> "List"
    endOfLine
    pure o

parseDir :: Parser Dir
parseDir = choice (map (\d -> let s = dirString d in d <$ string (T.pack s) <?> s) [minBound..maxBound]) <?> "dir"


pword = takeWhile1 (\c -> isLetter c || isPunctuation c)

-- ** Examples
exPrompt = "== Storage ==\
\\nThe boxes just contain more boxes.  Recursively.\
\\n\
\\nDoors here lead:\
\\n- north\
\\n- east\
\\n\
\\nItems here:\
\\n- boulder\
\\n\
\\nCommand?"

exPrompt2="== Hull Breach ==\
\\nYou got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed.\
\\n\
\\nDoors here lead:\
\\n- north\
\\n- east\
\\n- south\
\\n\
\\nCommand?"

exPrompt3="\
\\n\
\\n== Pressure-Sensitive Floor ==\
\\nAnalyzing...\
\\n\
\\nDoors here lead:\
\\n- west\
\\n\
\\nA loud, robotic voice says \"Alert! Droids on this ship are heavier than the detected value!\" and you are ejected back to the checkpoint.\
\\n\
\\n\
\\n\
\\n== Security Checkpoint ==\
\\nIn the next room, a pressure-sensitive floor will verify your identity.\
\\n\
\\nDoors here lead:\
\\n- north\
\\n- east\
\\n\
\\nCommand?\
\\n"

exPrompt4="\nYou take the astronaut ice cream.\n\nCommand?\n"
exPrompt5="\n\n\n== Pressure-Sensitive Floor ==\nAnalyzing...\n\nDoors here lead:\n- west\n\nA loud, robotic voice says \"Analysis complete! You may proceed.\" and you enter the cockpit.\nSanta notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.\n\"Oh, hello! You should be able to get in by typing 2236672 on the keypad at the main airlock.\"\n\n"


-- * Lenses
-- :set -ddump-splices
-- makeLenses ''Bot
computer :: Lens' Bot Computer
computer f_auLD (Bot x1_auLE x2_auLF x3_auLG x4_auLH)
  = (fmap (\ y1_auLI -> (((Bot x1_auLE) x2_auLF) x3_auLG) y1_auLI))
      (f_auLD x4_auLH)
{-# INLINE computer #-}
inventory :: Lens' Bot (Set Item)
inventory f_auLJ (Bot x1_auLK x2_auLL x3_auLM x4_auLN)
  = (fmap (\ y1_auLO -> (((Bot x1_auLK) x2_auLL) y1_auLO) x4_auLN))
      (f_auLJ x3_auLM)
{-# INLINE inventory #-}
outlook :: Lens' Bot (Map Coord Room)
outlook f_auLP (Bot x1_auLQ x2_auLR x3_auLS x4_auLT)
  = (fmap (\ y1_auLU -> (((Bot x1_auLQ) y1_auLU) x3_auLS) x4_auLT))
      (f_auLP x2_auLR)
{-# INLINE outlook #-}
position :: Lens' Bot Coord
position f_auLV (Bot x1_auLW x2_auLX x3_auLY x4_auLZ)
  = (fmap (\ y1_auM0 -> (((Bot y1_auM0) x2_auLX) x3_auLY) x4_auLZ))
      (f_auLV x1_auLW)
{-# INLINE position #-}

lItems :: Lens' Room (Set Item)
lItems f_auLD (Room x1_auLE x2_auLF x3_auLG x4_auLH)
  = (fmap (\ y1_auLI -> (((Room x1_auLE) x2_auLF) x3_auLG) y1_auLI))
      (f_auLD x4_auLH)
{-# INLINE lItems #-}