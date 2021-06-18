-- QuickCheck 7.3
myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [ ] = error "No tail for empty list!!"
-- >>> myTail [1,2,3,4]
-- [2,3,4]
-- >>> myTail [ ]
-- *** Exception: No tail for empty list!!
-- CallStack (from HasCallStack):
--   error, called at /home/regina/haskell/GetProgrammingWithHaskell/unit1/lesson7/myTail.hs:2:14 in main:Main
--

-- s7.1
-- Greatest Common Divisor (GCD) by Euclid. Even worst case where both a & b are prime will everntually have 1 as GCD.

myGCD a b = if remainder == 0
           then b
           else myGCD b remainder
     where remainder = a `mod` b