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
