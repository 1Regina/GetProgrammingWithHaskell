-- taken from unit3/lesson19/maybeExamples.hs
import qualified Data.Map as Map
import Data.Maybe
import Data.List
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- >>> Map.lookup 13 organCatalog
-- Just Brain

-- An empty case
-- >>> Map.lookup 3 organCatalog
-- Nothing

-- From unit3/lesson18/paramTypes.hs to access count of inventory items
-- 1. Define a range of keys:
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]
--  2. function to get the contents of each drawer with the lookup function:
-- note the intro of Maybe in [Maybe Organ]
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids where
                        getContents = \id -> Map.lookup id catalog

-- >>> getDrawerContents [7,14, 16, 28] organCatalog
-- [Just Heart,Just Spleen,Nothing,Nothing]


modelResults :: [Maybe Organ]
modelResults = [(Just Brain),Nothing,Nothing,(Just Spleen)]

-- NB: model answer sample list is different from getDrawerContents
-- Q19.1    Write a function emptyDrawers that takes the output of getDrawerContents and tells you the number of drawers that are empty.
emptyDrawers :: [Maybe Organ]  -> Int
emptyDrawers getDrawerContents = (length . filter isNothing) getDrawerContents

-- >>> filter isNothing getDrawerContents
-- Couldn't match expected type ‘[Maybe a]’
--             with actual type ‘[Int] -> Map Int Organ -> [Maybe Organ]’

-- >>> emptyDrawers getDrawerContents
-- Couldn't match expected type ‘[Maybe Organ]’
--             with actual type ‘[Int] -> Map Int Organ -> [Maybe Organ]’


-- >>> emptyDrawers modelResults
-- 2

-- Q19.2    Write a version of map that works for Maybe types, called maybeMap.

-- map :: (a -> b) -> [a] -> [b]
maybeMap :: (a -> b) ->  Maybe a -> Maybe b
maybeMap function Nothing = Nothing
maybeMap function (Just a) = Just (function a)