import qualified Data.Map as Map
type UserName = String
type GamerId = Int
type PlayerCredits = Int

-- Task 1: a function (creditsFromId) that will look up PlayerCredits given a GamerId with Maybe PlayerCredits to handle missing GamerId or there’s a missing entry for your GamerID - writing a wrapper for lookupCredits to be a function of Maybe UserName -> Maybe PlayerCredits.

--1. Map represents the database to get UserName from, given a GamerId.
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]
--2. This Map represents the credits database; you’ll use the UserName to look up PlayerCredits.
creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

--3. combine two Map.lookup functions.
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- 3A. lookupUserName function will take a GamerID and give you a Maybe UserName result
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB
-- 3B. lookupCredits function will take a User-Name and give the user a Maybe Credits result.
lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- 3C.  connect the result of lookupUserName (Maybe Username), with the function lookupCredits, UserName -> Maybe PlayerCredits
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

-- Applicative f => f a -> (a -> f b) -> f b

--  altLookupCredits, a solution without using Functor or Applicative
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- >>> creditsFromId 1
-- Just 2000
--------------------
-- Quick  check  30.1     Interestingly  enough,  the  following  function  seems  to  do  what  you  want and compiles just fine. What’s the issue? (Hint: Look at its type signature in GHCi.)

creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
-- creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits). It returns a strange Maybe (Maybe PlayerCredits) ~ nested/recursive Maybe
-- >>> lookupUserName 3
-- Just "dagon1997"
-- >>> creditsFromIdStrange 3
-- Just (Just 300)


-- Task 2: reads in user input and immediately prints it back. need a function that combines getLine and putStrln and returns an IO String
-- getLine :: IO String
-- putStrLn :: String -> IO ()
-- echo :: IO ()

-- IO String -> (String -> IO ()) -> IO () -- is abstract for below
-- Applicative f => f a -> (a -> f b) -> f b -- Applicative fails. This needs Monad bcos initial arg isnt in a context but its result is.
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Quick check 30.2    Why can’t you write a function like creditsFromId to solve this problem?

-- recall
-- altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
-- altLookupCredits Nothing = Nothing
-- altLookupCredits (Just username) = lookupCredits username

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)

-- QC 30.2 answer  You have no way of getting a value out of an IO context as you do a Maybe context.You need more powerful tools such as Applicative and Functor to work with IO types.

-- continue from 3C:
-- 4. Imagine your mobile gaming company was pur-chased by WillCo Industries, and now each GamerId is itself associated with a WillCoId.

type WillCoId = Int
gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB
-- 5.   need a new function, creditsFromWCId, of type WillCoId -> Maybe PlayerCredits. You can easily create this by chaining all three of your lookup functions with >>=
creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits
-- >>> creditsFromWCId 1001
-- Just 2000
