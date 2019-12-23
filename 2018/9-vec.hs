module Main where

import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.Environment

main = do
    [players,last_ball] <- map read <$> getArgs
    print (answer players last_ball)
    
answer :: Int -> Int -> Int
answer num_players last_marble = V.maximum $ scores $ playGame (Game num_players last_marble)

data Game = Game
    { num_players :: !Int
    , last_marble :: !Int
    } deriving (Show, Eq)
data Position = Position
    { position    :: !(V.Vector Int)
    , curr_index  :: !Int
    , scores      :: !(V.Vector Int)
    } deriving (Show, Eq)
    
-- player 0 exists, assume >= 5 players
-- should use next_ball = 5, curr_player = 4
initialPosition :: Game -> Position
initialPosition game = Position
    { position    = V.fromList [0,4,2,1,3]
    , curr_index  = 1
    , scores      = V.replicate (num_players game) 0
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
    , curr_index  = next_index `mod` V.length new_position
    , scores      = new_scores
    }
    where
    next_index = (curr_index pos - 7) `mod` V.length (position pos)
    (before,after) = V.splitAt next_index (position pos)
    new_position = V.force $ before V.++ V.tail after
    picked_up = V.head after
    new_scores = V.modify (\v -> VM.modify v (+(next_ball + picked_up)) curr_player) (scores pos)
move curr_player next_ball pos = Position
    { position    = v_insert next_index next_ball (position pos)
    , curr_index  = next_index
    , scores      = scores pos
    }
    where 
    next_index = (curr_index pos + 2) `mod` V.length (position pos)

v_insert :: VM.Unbox a => Int -> a -> V.Vector a -> V.Vector a
v_insert i a v = 
    let (before,after) = V.splitAt i v
     in V.force $ before V.++ V.cons a after
    
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