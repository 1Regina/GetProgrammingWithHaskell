import Data.Char (toLower)
import qualified Data.Map as Map

-- Q18.1    For the types Triple and Box, implement a function similar to map, tripleMap, and boxMap

data Triple a = Triple a a a deriving Show

newtype Box a = Box a deriving Show

-- recall
data List a = Empty | Cons a (List a) deriving Show
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest)  = Cons (func a) (ourMap func rest)

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)


tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple x1 x2 x3) = Triple (func x1) (func x2) (func x3)

boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box x) = Box (func x)

-- Q18.2    Modify the Organ type so that it can be used as a key. Then build a Map, organ-Inventory, of each organ to its count in the organCatalog.

-- From previous in unit3/lesson18/paramTypes.hs
organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs
-- >>> organPairs
-- [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
-- >>> organCatalog
-- fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

-- 1. make Organs the key of Map so make it Ord
-- You add enum to easily build a list of all organs like in Die
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)
-- 2. Unmap things to a list of tuples
-- toList :: Map k a -> [(k, a)]
-- >>> Map.toList organCatalog
-- [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

-- 3. Step 2 + extract the second values in each tuple inside list and put into a list (rem map :: (a -> b) -> [a] -> [b])
organNames :: [Organ]
organNames = map snd (Map.toList organCatalog)
-- >>> organNames
-- [Heart,Heart,Brain,Spleen,Spleen,Kidney]

-- 4. count each organ into a list according to their position
organCounts :: [Int]
organCounts = map countOrgan organNames  where
    countOrgan = (\organ ->(length . filter (== organ)) organNames)

-- >>> organCounts
-- [2,2,1,2,2,1]

-- 5. create the inventory count by first zipping the organNames and organdCounts lists together into a common list of lists with duplicates.
organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip organNames organCounts)
-- >>> organInventory
-- fromList [(Heart,2),(Brain,1),(Kidney,1),(Spleen,2)]
