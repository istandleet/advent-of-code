{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language NoMonomorphismRestriction #-}
module Main where 

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Data.List

import Data.Char
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main = do
    putStrLn "24"
    fight <- getInput
    print $ runFight fight
    let b = findBoost fight 
    print b
    print $ runFight $ boost b fight
    
getInput :: IO Fight
getInput = readFile "24.txt"
      >>= either fail return . parseOnly parseFight . T.pack
   

findBoost :: Fight -> Int
findBoost f = findBounds $ \i -> fst $ runFight $ boost i f 

data Bounds = Bounds 
   { lower  :: {-# unpack #-} !Int
   , upper  :: {-# unpack #-} !Int
   } deriving (Eq,Show)
   
findBounds :: (Int -> Bool) -> Int
findBounds f = go (Bounds 0 (-1))
  where
  go (Bounds l u)
        | l == u - 1 = u
        | u < 0 = go $ Bounds l $ if f (negate u) then negate u else 2*u
        | otherwise -- f u, not (f l)
            = go $ let m = (l + u) `div` 2
                    in if f m then Bounds l m else Bounds m u
   
boost :: Int -> Fight -> Fight
boost a = lImmuneSystem %~ map (lAttackDamage %~ (+a)) 
      
data Fight = Fight
   { immuneSystem :: [Group]
   , infection    :: [Group]
   } deriving (Eq,Show)
   
lImmuneSystem :: Lens' Fight [Group]
lImmuneSystem f (Fight a b) = fmap (\a' -> Fight a' b) (f a)
lInfection    :: Lens' Fight [Group]
lInfection    f (Fight a b) = fmap (\b' -> Fight a b') (f b)
   
data Group = Group
   { unitCount  :: {-# unpack #-} !Int
   , hitPoints  :: {-# unpack #-} !Int
   , immunities :: [Text]
   , weaknesses :: [Text]
   , attackDamage :: {-# unpack #-} !Int
   , attackType :: Text
   , initiative :: {-# unpack #-} !Int
   } deriving (Eq,Show)
   
effectivePower :: Group -> Int
effectivePower g = unitCount g * attackDamage g
   
lUnitCount :: Lens' Group Int
lUnitCount f g = fmap (\uc' -> g {unitCount = uc'}) (f $ unitCount g)
lAttackDamage :: Lens' Group Int
lAttackDamage f g = fmap (\ad' -> g {attackDamage = ad'}) (f $ attackDamage g)
   
damageTo :: Group -> Group -> Int
g1 `damageTo` g2 
    | attackType g1 `elem` immunities g2 = 0
    | attackType g1 `elem` weaknesses g2 = 2*effectivePower g1
    | otherwise = effectivePower g1
attackingOrd :: Group -> Group -> (Int,Int,Int)
attackingOrd g1 g2 = (g1 `damageTo` g2, effectivePower g2, initiative g2)

chooseTarget :: Group -> [Int] -> [Group] -> Maybe Int
chooseTarget g1 is gs = fmap fst $ listToMaybe $ Data.List.sortOn (Down . attackingOrd g1 . snd)
    [ (i,g2)
    | (i,g2) <- zip [0..] gs
    , i `notElem` is
    , attackType g1 `notElem` immunities g2
    ]
    
runFight :: Fight -> (Bool,Int)
runFight fight 
    | null $ infection    fight = (True ,sum $ map unitCount $ immuneSystem fight)
    | null $ immuneSystem fight = (False,sum $ map unitCount $ infection    fight)
    | otherwise = runFight $ execState runRound fight
    
runRound :: State Fight ()
runRound = do
    ts <- targetSelection <$> get
    attacking ts
    
targetSelection :: Fight -> ([Maybe Int],[Maybe Int])
targetSelection Fight{..} = (ims,inf)
    where 
    ims = map snd $ Data.List.sort $ foldl (go True ) [] $ pickOrder $ zip [0..] immuneSystem
    inf = map snd $ Data.List.sort $ foldl (go False) [] $ pickOrder $ zip [0..] infection
    
    go isImmuneSystem is (i,g1) = (i,chooseTarget g1 (catMaybes $ map snd is) opposition) : is
        where
        opposition = if isImmuneSystem then infection else immuneSystem
    pickOrder = Data.List.sortOn (Down . ((,) <$> effectivePower <*> initiative) . snd)
    
attacking :: ([Maybe Int],[Maybe Int]) -> State Fight ()
attacking (ims,inf) = do
    order <- fightOrder <$> get
    forM_ order $ uncurry $ \isImmuneSystem i ->
        modify $ attack isImmuneSystem i $ if isImmuneSystem then ims !! i else inf !! i
    modify $ \f -> f 
           & lImmuneSystem %~ filter ((>0) . unitCount)
           & lInfection %~ filter ((>0) . unitCount)

fightOrder :: Fight -> [(Bool, Int)]
fightOrder fight = map snd $ Data.List.sortOn Down
    [ (initiative g,(b,i))
    | (b,(i,g)) <- map (True ,) (zip [0..] $ immuneSystem fight)
                ++ map (False,) (zip [0..] $ infection fight)
    ]

attack :: Bool -> Int -> Maybe Int -> Fight -> Fight
attack isImmuneSystem i Nothing f = f
attack isImmuneSystem i (Just j) f 
    | unitCount g1 < 1 = f
    | otherwise = f & opposition %~ ix j %~ change
    where
    g1 = thisside f !! i
    change g2 = 
        let dmg = g1 `damageTo` g2
            lol = dmg `div` hitPoints g2
         in g2 & lUnitCount %~ (subtract lol)
    opposition = if     isImmuneSystem then lInfection else lImmuneSystem
    thisside   = if not isImmuneSystem then  infection else  immuneSystem
    
    
parseFight :: Parser Fight
parseFight = do
    string' "Immune System:\n"
    immuneSystem <- sepBy1' parseGroup (char '\n')
    skipSpace
    string' "Infection:\n"
    infection <- sepBy1' parseGroup (char '\n')
    return Fight{..}
    where
    string' s = string s <?> T.unpack s
   
parseGroup :: Parser Group
parseGroup = do
    -- 989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
    unitCount <- decimal
    string' " units each with "
    hitPoints <- decimal
    string' " hit points "
    (immunities,weaknesses) <- parseparens
    skipSpace
    string' "with an attack that does "
    attackDamage <- decimal
    char ' '
    attackType <- word
    string' " damage at initiative "
    initiative <- decimal
    return Group{..}
    where
    parseparens = option mempty $ do
        char '('
        e <- eitherP parseImmunities parseWeaknesses
        case e of 
            Left  immunities -> do
                optional "; "
                weaknesses <- option [] parseWeaknesses
                char ')'
                return (immunities,weaknesses)
            Right weaknesses -> do
                optional "; "
                immunities <- option [] parseImmunities
                char ')'
                return (immunities,weaknesses)
        
    parseImmunities = do
        string' "immune to "
        sepBy1' word ", "
    parseWeaknesses = do
        string' "weak to "
        sepBy1' word ", "
    
    word = takeWhile1 isAlpha
    string' s = string s <?> T.unpack s
    
s = "Immune System:\n\
\17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n\
\989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\
\\n\
\Infection:\n\
\801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n\
\4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"