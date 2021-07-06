
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

-- data Maybe a = Nothing | Just a

-- >>> Map.lookup 13 organCatalog
-- Just Brain

-- >>> :t Map.lookup 13 organCatalog
-- Map.lookup 13 organCatalog :: Maybe Organ

-- An empty case
-- >>> Map.lookup 3 organCatalog
-- Nothing

-- Quick check 19.1 type of `Nothing` in here is also Maybe Organ
-- >>> :t Map.lookup 3 organCatalog
-- Map.lookup 3 organCatalog :: Maybe Organ


-- From unit3/lesson18/paramTypes.hs to access count of inventory items
-- 1. Define a range of keys:
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]
--  2. function to get the contents of each drawer with the lookup function:
-- note the intro of Maybe in [Maybe Organ]
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids where
                        getContents = \id -> Map.lookup id catalog

-- >>> getDrawerContents [7,14, 16] organCatalog
-- [Just Heart,Just Spleen,Nothing]

-- --  3. list of available organs inclusive those with missing values
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- >>> availableOrgans
-- [Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Nothing,Just Brain,Just Spleen,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just Spleen,Nothing,Nothing,Just Kidney,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]

-- 4. Count instances of organs
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                     (\x -> x == Just organ)
                                     available)

--  Maybe implements `Eq`, so you can just compare two Maybe Organs.
-- >>> countOrgan Brain availableOrgans
-- 1
-- >>> countOrgan Spleen availableOrgans
-- 2
-- >>> countOrgan Eye availableOrgans
-- Data constructor not in scope: Eye :: Organ


-- 19.3 Computing with Maybe

-- B1. filter availableOrgans for when it is not Nothing
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

-- B2 Using isSomething with filter to clean [Maybe Organ]
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans
-- >>> justTheOrgans
-- [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]

-- B2 ALTERNATIVE
justTheOrgans' :: [Maybe Organ]
justTheOrgans' = filter isJust availableOrgans
-- >>> justTheOrgans
-- [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]

-- B3 Apply showOrgan to remove Just
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""
-- >>> showOrgan (Just Heart)
-- "Heart"

-- B4: use map to create a list without Nothing and Organs without Just
organList :: [String]
organList = map showOrgan justTheOrgans
-- >>> organList
-- ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]

-- B5: insert commas with `intercalate`
cleanList :: String
cleanList = intercalate ", " organList
-- >>> cleanList
-- "Heart, Heart, Brain, Spleen, Spleen, Kidney"


-- Quick check 19.2
numOrZero :: Maybe Int -> Int
numOrZero (Just n) = n
numOrZero Nothing = 0

-- 19.4 Computation with Maybe
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
      show (Vat organ) = show organ ++ " in a vat"
      show (Cooler organ) = show organ ++ " in a cooler"
      show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ =  placeInLocation (organToContainer organ)
-- >>> process Brain
-- (Lab,Brain in a vat)
-- >>>  process Spleen
-- (Kitchen,Spleen in a bag)

report ::(Location,Container) -> String
report (location,container) = show container ++" in the " ++ show location
-- >>> report (process Brain)
-- "Brain in a vat in the Lab"
-- >>> report (process Spleen)
-- "Spleen in a bag in the Kitchen"

-- write a function similar to below but deal with Maybe Organ
-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ) where
--                     organ = Map.lookup id catalog

-- combine report and process into a function that handles the Maybe Organ
processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport  Nothing = "error, id not found"

-- now we are ready to use the processRequest without the maybe
-- compare with report (process someOrgan), processRequest take an drawer index n catalog
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ where
                    organ = Map.lookup id catalog

-- >>> processRequest 13 organCatalog
-- "Brain in a vat in the Lab"


-- Quick check 19.3
reportMaybe :: Maybe  (Location,Container) -> String
reportMaybe Nothing = " no container as organ is missing"
reportMaybe (Just (location, container)) = show container
                                            ++ " in the "
                                            ++ show location