-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6


--ERROR
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- show S3 = "three"
-- show S4 = "four"
-- show S5 = "five"
-- show S6 = "six"

-- >>> S1 
-- <interactive>:477:2-3: error:
--     • No instance for (Show SixSidedDie) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
-- >>> show S1
-- <interactive>:604:2-5: error:
--     Ambiguous occurrence ‘show’
--     It could refer to
--        either ‘Prelude.show’,
--               imported from ‘Prelude’ at /home/regina/smu/haskell/ProgrammingHaskell/unit2/lesson14/createTypeClasses.hs:1:1
--               (and originally defined in ‘GHC.Show’)
--            or ‘Main.show’,
--               defined at /home/regina/smu/haskell/ProgrammingHaskell/unit2/lesson14/createTypeClasses.hs:12:1
--
--CORRECT
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 
instance Show SixSidedDie where
   show S1 = "I"   
   show S2 = "II"   
   show S3 = "III"   
   show S4 = "IV"   
   show S5 = "V"   
   show S6 = "VI"

   -- >>> S1
   -- I
   -- >>> show S5 (confusion)
   -- (Error while loading modules for evaluation)



-- QuickCheck 14.2
-- RealFrac : Minimal complete definition: properFraction, round, floor and ceiling from Hoogle
-- From Hackage.haskell.org: properFraction :: Integral b => a -> (b, a)

-- The function properFraction takes a real fractional number x and returns a pair (n,f) such that x = n+f, and:

-- n is an integral number with the same sign as x; and
-- f is a fraction with the same type and sign as x, and with absolute value less than 1.
-- The default definitions of the ceiling, floor, truncate and round functions are in terms of properFraction.

--QuickChek 14.3
-- With S5 and S6 completed,
-- compare S4 S4 = EQ
-- compare S4  _ = GT
-- compare _  S4 = LT

--QuickCheck 14.4

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord)