import Data.List
-- Q29.1    To    prove    that    Applicative is strictly more powerful than Functor, write a universal version of fmap, called allFmap, that defines fmap for all members of the Applicative type class. Because it works for all instances of Applicative, the only functions you can use are the methods required by the Applicative type class. To get you started, here’s your type signature:

allFmap :: Applicative f => (a -> b) -> f a -> f b

-- When you’re finished, test this out on List and Maybe, which are both members of Applicative:
-- GHCi> allFmap (+ 1) [1,2,3]
-- [2,3,4]
-- GHCi> allFmap (+ 1) (Just 5)
-- Just 6
-- GHCi> allFmap (+ 1) Nothing
-- Nothing

--                  Recall         pure (+)     <*> [1000,2000,3000] <*> [500,20000]
allFmap function applicInstance = pure function <*> applicInstance
-- >>> allFmap (+ 1) [1,2,3]
-- [2,3,4]

-- >>> allFmap (+ 1) (Just 5)
-- Just 6
-- >>> allFmap (+ 1) Nothing
-- Nothing

-- >>> fmap (+ 1) [1,2,3]
-- [2,3,4]
-- >>> fmap (+ 1) (Just 5)
-- Just 6


-- Q29.2    Translate the following expression into one where the result is a Maybe Int. The catch is that you may not add (or remove) anything to the code except pure and <*>. You can’t use the Just constructor or any extra parentheses.

example :: Int
example = (*) ((+) 2 4) 6
-- >>> example
-- 36
-- Here’s the type signature for your answer:
exampleMaybe :: Maybe Int
-- >>> pure (6+) <*> Just 5 -- recall
-- Just 11
-- >>> pure (+) <*> pure 2 <*> pure 4
-- 6
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6
-- >>> exampleMaybe
-- Just 36

-- Q29.3    Take the following example and use nondeterministic computing with Lists to determine how much beer you need to purchase to assure there will be enough:
-- 1. You bought beer last night but don’t remember whether it was a 6-pack or a 12-pack.
-- 2. You and your roommate each had two beers last night.
-- 3. You’re having either two or three friends coming over tonight, depending on who can come.
-- 4. For a long night of gaming, you expect the average person to drink three to four beers.
-- recall
-- doorPrize :: [Int]
-- doorPrize = [1000,2000,3000]
-- 1. You bought beer last night but don’t remember whether it was a 6-pack or a 12-pack.
existingBeer :: [Int]
existingBeer = [6,12]
-- 2. You and your roommate each had two beers last night.
-- drankLastNight = - 4
remainingBeer :: [Int]
remainingBeer = fmap (\inventory -> inventory -4) existingBeer  -- or (\inventory -> inventory -4) <$>existingBeer
-- >>> remainingBeer
-- [2,8]

-- 3. You’re having either two or three friends coming over tonight, depending on who can come.
coming :: [Int]
coming = [2,3]

drinkers :: [Int]
drinkers = pure (+2) <*> coming
-- >>> drinkers
-- [4,5]

-- 4. For a long night of gaming, you expect the average person to drink three to four beers.
perPaxDrinks :: [Int]
perPaxDrinks = [3,4]

totalBeerToBuy:: [Int]
totalBeerToBuy = pure (-) <*> (pure (*) <*> drinkers <*>  perPaxDrinks)  <*> remainingBeer

-- >>> totalBeerToBuy
-- [10,4,14,8,13,7,18,12]

-- Textbook solution
totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*>  perPaxDrinks <*> drinkers
beersToPurchase :: [Int]
beersToPurchase = (pure (-)) <*> totalBeersNeeded  <*> remainingBeer
-- >>> beersToPurchase
-- [10,4,13,7,14,8,18,12]

-- >>> sort beersToPurchase == sort totalBeerToBuy
-- True
