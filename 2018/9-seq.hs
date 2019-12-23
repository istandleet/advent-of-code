module Main where

import Control.Monad.State.Strict
import Data.Foldable 
import Data.Sequence as S hiding (length, zip, drop)
import qualified Data.IntMap.Strict as IntMap
import System.Environment

main = do
    [players,last_ball] <- map read <$> getArgs
    print (answer players last_ball)
    
answer :: Int -> Int -> Int
answer num_players last_marble = maximum $ scores $ playGame (Game num_players last_marble)

-- 430 71588 == 422748
test = do
    print (answer 10 1618 == 8317)
    print (answer 13 7999 == 146373)
    print (answer 17 1104 == 2764)
    print (answer 21 6111 == 54718)
    print (answer 30 5807 == 37305)

data Game = Game
    { num_players :: !Int
    , last_marble :: !Int
    } deriving (Show, Eq)
data Position = Position
    { position    :: !(Seq Int)
    , curr_index  :: !Int
    , scores      :: !(IntMap.IntMap Int)
    } deriving (Show, Eq)
    
-- player 0 exists, assume >= 5 players
-- should use next_ball = 5, curr_player = 4
initialPosition :: Game -> Position
initialPosition game = Position
    { position    = fromList [0,4,2,1,3]
    , curr_index  = 1
    , scores      = IntMap.fromList $ zip [0..num_players game - 1] $ repeat 0
    }

playGame :: Game -> Position
playGame game = foldl' go (initialPosition game) (inputs game) 
    where
    go pos (curr_player,next_ball) = move curr_player next_ball pos

inputs :: Game -> [(Int,Int)]
inputs game = drop 4 $ zip (cycle [0..num_players game - 1]) [1..last_marble game]
    
move :: Int -> Int -> Position -> Position
move curr_player next_ball pos | next_ball `mod` 23 == 0 = Position
    { position    = new_position
    , curr_index  = next_index `mod` length new_position
    , scores      = new_scores
    }
    where
    next_index = (curr_index pos - 7) `mod` length (position pos)
    picked_up    = position pos `S.index` next_index 
    new_position = S.deleteAt next_index (position pos)
    new_scores   = IntMap.adjust (+(next_ball + picked_up)) curr_player (scores pos)
move curr_player next_ball pos = Position
    { position    = insertAt next_index next_ball (position pos)
    , curr_index  = next_index
    , scores      = scores pos
    }
    where 
    next_index = (curr_index pos + 2) `mod` length (position pos)

{-    
position = [0,4,2,1,3]
curr_player = 4 # player 0 exists, assume >= 5 players
curr_index  = 1
next_ball   = 5
scores = [0]*num_players

while next_ball <= last_marble:
    if next_ball % 23 != 0:
        next_index = (curr_index + 2) % len(position)
        position.insert(next_index,next_ball)
        curr_index = next_index
    else:
        scores[curr_player] += next_ball
        next_index = (curr_index - 7) % len(position)
        ball_picked_up = position.pop(next_index)
        scores[curr_player] += ball_picked_up
        curr_index = next_index % len(position)
    next_ball += 1
    curr_player = (curr_player + 1) % num_players
max(scores)
-}