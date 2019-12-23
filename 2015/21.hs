{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
module Main where 

import Data.Monoid

main = do
    putStrLn "20"
    print $ minimum
        [playerCost p | p <- allPlayers, runBattle p (109,8,2)]
    print $ maximum
        [playerCost p | p <- allPlayers, not $ runBattle p (109,8,2)]

runBattle :: Player -> (Int,Int,Int) -> Bool
runBattle p = let (Sum a,Sum d) = playerStats p in battle (100,a,d) 
    
battle :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
battle (hp,damage,defense) (hp',damage',defense')
    | hp' <= 0 = True
    | hp  <= 0 = False
    | otherwise = battle (hp1,damage,defense) (hp1',damage',defense')
    where
    hp1  = hp  - ((damage' - defense ) `max` 1)
    hp1' = hp' - ((damage  - defense') `max` 1)
    
data Player = Player
    { weapon :: Weapon
    , armor  :: Maybe Armor
    , lring  :: Maybe Ring
    , rring  :: Maybe Ring
    } deriving (Ord,Show,Eq)
    
playerStats :: Player -> Stats
playerStats Player{..} =
    weaponStats weapon <>
    maybe mempty armorStats armor <>
    maybe mempty ringStats lring <>
    maybe mempty ringStats rring
playerCost :: Player -> Int
playerCost Player{..} =
    weaponCost weapon +
    maybe 0 armorCost armor +
    maybe 0 ringCost lring  +
    maybe 0 ringCost rring
    
data Weapon = 
     Dagger     
   | Shortsword 
   | Warhammer  
   | Longsword  
   | Greataxe 
   deriving (Ord, Show, Eq, Bounded,Enum)
   
weaponStats = \case 
    Dagger     -> (4,0)
    Shortsword -> (5,0)
    Warhammer  -> (6,0)
    Longsword  -> (7,0)
    Greataxe   -> (8,0)
weaponCost = \case 
    Dagger     -> 8
    Shortsword -> 10
    Warhammer  -> 25
    Longsword  -> 40
    Greataxe   -> 74
   
data Armor = 
     Leather     
   | Chainmail   
   | Splintmail  
   | Bandedmail  
   | Platemail   
   deriving (Ord, Show, Eq, Bounded,Enum)
   
armorStats = \case
    Leather     -> (0,1)
    Chainmail   -> (0,2)
    Splintmail  -> (0,3)
    Bandedmail  -> (0,4)
    Platemail   -> (0,5)
armorCost = \case
    Leather     -> 13
    Chainmail   -> 31
    Splintmail  -> 53
    Bandedmail  -> 75
    Platemail   -> 102
    
data Ring = 
     Damage1
   | Damage2
   | Damage3
   | Defense1
   | Defense2
   | Defense3
   deriving (Ord, Show, Eq, Bounded,Enum)
   
ringStats = \case
    Damage1  -> (1,0)
    Damage2  -> (2,0)
    Damage3  -> (3,0)
    Defense1 -> (0,1)
    Defense2 -> (0,2)
    Defense3 -> (0,3)
ringCost = \case
    Damage1  -> 25
    Damage2  -> 50
    Damage3  -> 100
    Defense1 -> 20
    Defense2 -> 40
    Defense3 -> 80
   
type Stats = (Sum Int, Sum Int)

allPlayers :: [Player]
allPlayers = 
    [ Player a b c d
    | a  <- [minBound..maxBound]
    , b  <- Nothing : map Just [minBound..maxBound]
    , c  <- Nothing : map Just [minBound..maxBound]
    , d  <- dlist c
    ]
    where
    dlist (Just t) 
        | t == maxBound = []
        | otherwise = map Just [succ t..maxBound]
    dlist _ = Nothing : map Just [minBound..maxBound]

{-
data EnumPair a = EnumPair {ffst :: a, ssnd :: a} deriving (Eq,Show,Ord)
instance (Ord a, Enum a, Bounded a) => Enum (EnumPair a) where
    -- succ (EnumPair a b)
    --     | b == maxBound = EnumPair (succ a) (succ (succ a))
    --     | otherwise = EnumPair a (succ b)
    -- pred (EnumPair a b)
    --     | b == succ a = EnumPair (pred a) maxBound
    --     | otherwise = EnumPair a (pred b)
        
    fromEnum (EnumPair a b) = (fromEnum a*fromEnum(maxBound :: a)+fromEnum b) - f (fromEnum a+1)
        where f n = ((n^2-n) `div` 2) + 1
    toEnum i = [EnumPair a b | a <- [minBound..pred maxBound], b <- [succ a..maxBound]] !! i
    
instance (Ord a, Enum a, Bounded a) => Bounded (EnumPair a) where
    minBound = EnumPair minBound (succ minBound)
    maxBound = EnumPair (pred maxBound) maxBound
-}