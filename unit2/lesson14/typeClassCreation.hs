-- Incorrect without Show.
-- data SixSidedDieWOShow = S1 | S2 | S3 | S4 | S5 | S6
-- -- >>> S1
-- -- No instance for (Show SixSidedDieWOShow)
-- --   arising from a use of ‘evalPrint’


-- Correct but boring
-- data SixSidedDieWShow = S1 | S2 | S3 | S4 | S5 | S6  deriving (Show)
-- >>> S1
-- S1
-- >>> S4
-- S4

-- Correct and print a string. Remove deriving show
-- data SixSidedDieEng = S1 | S2 | S3 | S4 | S5 | S6  -- remove deriving (Show) bcos of show instance
-- instance Show SixSidedDieEng where
--    show S1 = "one"
--    show S2 = "two"
--    show S3 = "three"
--    show S4 = "four"
--    show S5 = "five"
--    show S6 = "six"

-- >>> S1
-- one

-- Quick check 14.1
-- data SixSidedDieRom = S1 | S2 | S3 | S4 | S5 | S6  -- remove deriving (Show) bcos of show Instance
-- instance Show SixSidedDieRom where
--    show S1 = "I"
--    show S2 = "II"
--    show S3 = "III"
--    show S4 = "IV"
--    show S5 = "V"
--    show S6 = "VI"
-- >>> S1
-- I

-- Eq type
-- data SixSidedDieEq = S1 | S2 | S3 | S4 | S5 | S6
-- instance Eq SixSidedDieEq where
--       (==) S6 S6 = True
--       (==) S5 S5 = True
--       (==) S4 S4 = True
--       (==) S3 S3 = True
--       (==) S2 S2 = True
--       (==) S1 S1 = True
--       (==) _ _ = False

-- >>> S6 == S6
-- True

-- >>> S5 /= S6
-- True

-- >>> S6 == S5
-- False

-- QuickCheck 14.2
-- RealFrac : Minimal complete definition: properFraction, round, floor and ceiling from Hoogle
-- From Hackage.haskell.org: properFraction :: Integral b => a -> (b, a)

-- The function properFraction takes a real fractional number x and returns a pair (n,f) such that x = n+f, and:

-- n is an integral number with the same sign as x; and
-- f is a fraction with the same type and sign as x, and with absolute value less than 1.
-- The default definitions of the ceiling, floor, truncate and round functions are in terms of properFraction.

-- adding Ord for SixSidedDieEq to data SixSidedDieEq and instance Eq SixSidedDieEq above
-- instance Ord SixSidedDieEq where
--       compare S6 S6 = EQ
--       compare S6 _ = GT
--       compare _ S6 = LT
--       compare S5 S5 = EQ
--       compare S5 _ = GT
--       compare _ S5 = L
-- -- Quick check 14.3 add patterns for case S4.
--       compare S4 S4 = EQ
--       compare S4 _ = GT
--       compare _ S4 = LT
-- Note:  Because  of  pattern  matching,  the  case  of  compare  S5  S4  and  compare  S6  S4  will  already  bematched

-- Quick check 14.4
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Ord, Eq)
