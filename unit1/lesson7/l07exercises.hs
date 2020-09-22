-- Q7.1
-- >>> tail []
-- *** Exception: Prelude.tail: empty list
--
myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [] = []
-- >>> myTail [1,2,3,4]
-- [2,3,4]
--

-- >>> myTail [ ]
-- []
--
-- Q7.2
-- 1) Identify the end goal(s).                                    => Largest GCD
-- 2) Determine what happens when a goal is reached.               => a `mod` gcd == 0 && b `mod` gcd == 0
-- 3) List all alternate possibilities.                            => a `mod` gcd == 0 && b `mod` gcd /= 0
--                                                                 => a `mod` gcd /= 0 && b `mod` gcd == 0
-- 4) Determine your “rinse and repeat” process.                   => GCD b `mod` (a `mod` b)
-- 5) Ensure that each alternative moves you toward the goal.



myGCD :: Integral t => t -> t -> t
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

-- >>> myGCD 9 0
-- 9
--

-- >>> myGCD 9 3
-- 3
--
