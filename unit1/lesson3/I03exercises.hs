calchange :: (Ord p, Num p) => p -> p -> p
calchange  = (\owed given->
                if owed > given
                    then owed - given
                 else 0)
-- >>> calchange 5 2
-- 3


inc :: Integer -> Integer
inc = (\x -> x + 1 )
-- >>>  inc 1
-- 2

double :: Integer -> Integer
double = (\x -> x * 2)
-- >>>  double 1
-- 2


square :: Integer -> Integer
square = (\x -> x ^ 2)
-- >>>  square 3
-- 9


counter :: Num a => a -> a
counter x  = (\x -> x + 1)
           (( \x -> x + 1 )
             ((\x -> x) x))

-- >>> counter 3
-- 5


counter1 :: Num a => a -> a
counter1 x = (\y -> y + 1) ((\y -> y + 1) x )

-- >>> counter1 3
-- 5

-- same as counter1
counter2 :: Num a => a -> a
counter2 x   = (\x -> x + 1)
               ((\x -> x + 1 )
               ((\x -> x) x))
-- >>> counter2 3
-- 5

-- removing x as argument would fail
counter3   = (\w -> w + 1)
              ((\w -> w + 1 )
               ((\w -> w)))
-- >>> counter3 3

-- take out this comment out section for counter4 x to end to work
-- counter3 fail bcos fn expecting a number output as input but there is none
-- ((\y -> y + 1) + 1)  --cannot +1 to a function as +1 is also a function
-- >>> counter3 3
-- <interactive>:6914:11: error:
--     • No instance for (Num GHC.Types.Any) arising from the literal ‘3’
--     • In the first argument of ‘counter3’, namely ‘3’
--       In the expression: counter3 3
--       In an equation for ‘it’: it = counter3 3


-- x as an argument is esssential for completion see above
-- counter4i x = (\y -> y + 1) $(\y -> y + 1) x
-- -- is not essentially
-- counter4ii x = (+ 1) $((+ 1)) x
-- which is not essentially
-- counter4iii = (+ 1) $((+ 1))
-- >>> counter4i 3
-- >>> counter4ii 3
-- >>> counter4iii 3
-- Variable not in scope: counter4i :: Integer -> t


counter4 :: Num a => a -> a
counter4 x = (\y -> y + 1) $(\y -> y + 1) x

-- >>> counter4 3
-- 5


plus1 :: Num a => a -> a
plus1 = (\y -> y + 1)
-- >>> plus1 1
-- 2

counter5 x = plus1 . plus1 $ x
-- >>> counter5 1
-- 3

-- this works bcos it is complete for every part
counter6 = (\y -> y + 1) . (\y -> y + 1)
-- >>>  counter6 1
-- 3
