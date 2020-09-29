aList :: [[Char]]
aList = ["cat","dog","mouse"]
-- >>> :t aList
-- <interactive>:1:1-5: error: Variable not in scope: aList
--
-- division is not included as a function in Num as div could return a float 
--division with (/) isn’t defined on all cases of Num.


data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)
-- >>> Chocolate == Vanilla
-- >>> Chocolate /= Vanilla
-- >>> Chocolate == Chocolate
-- >>> Vanilla == Vanilla
-- >>>  Chocolate < Vanilla
-- >>> Chocolate > Vanilla
-- False
-- True
-- True
-- True
-- True
-- False
--Haskell defaults to the order of the data constructors for determining Ord. So Vanilla will be greater than Chocolate.


-- >>> :info Show
