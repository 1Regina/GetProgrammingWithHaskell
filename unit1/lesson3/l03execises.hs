calchange :: (Ord p, Num p) => p -> p -> p
calchange  = (\owed given->
                if owed > given
                    then owed - given
                 else 0) 
-- >>> calchange 5 2
-- 3
--

inc :: Integer -> Integer
inc = (\x -> x + 1 )
-- >>>  inc 1
-- 2
double :: Integer -> Integer
double = (\x -> x * 2)
-- >>>  double 1
-- 2
--
square :: Integer -> Integer
square = (\x -> x ^ 2)
-- >>>  square 3
-- 9
--
-- counter :: ( Num p) => p -> p
counter x  = (\x -> x + 1)
           (( \x -> x + 1 )
             ((\x -> x) x))
-- >>> counter 3
-- 5
--

--
counter2 x   = (\x -> x + 1)
               ((\x -> x + 1 )
               ((\x -> x) x))
-- >>> counter2 3
-- 5
--

counter3   = (\w -> w + 1)
              ((\w -> w + 1 )
               ((\w -> w)))
-- fail bcos fn expecting a number output as input but there is none               
-- ((\y -> y + 1) + 1)  --cannot +1 to a function as +1 is also a function
-- >>> counter3 3
-- <interactive>:6914:11: error:
--     • No instance for (Num GHC.Types.Any) arising from the literal ‘3’
--     • In the first argument of ‘counter3’, namely ‘3’
--       In the expression: counter3 3
--       In an equation for ‘it’: it = counter3 3
--


counter1 :: Num a => a -> a
-- counter1  = (\y -> y + 1) . (\y -> y + 1) 
counter1 x = (\y -> y + 1) ((\y -> y + 1) x )

-- >>> counter1 3
-- 5
--
counter4 x = (\y -> y + 1) $(\y -> y + 1) x 

-- >>> counter4 3
-- 5
--

plus1 :: Num a => a -> a
plus1 = (\y -> y + 1)
counter5 x = plus1 . plus1 $ x


counter6 = (\y -> y + 1) $(\y -> y + 1)
-- >>>  counter6 3
-- <interactive>:147:3-10: error:
--     • Variable not in scope: counter6 :: Integer -> t
--     • Perhaps you meant one of these:
--         ‘counter’ (line 25), ‘counter2’ (line 33), ‘counter3’ (line 40)
--
