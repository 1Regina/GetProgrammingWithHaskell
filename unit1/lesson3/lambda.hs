-- >>> (\x -> x * 2) 4
-- 8
-- >>> (\x -> x * 2) 1
-- >>> (\x -> x * 2) 3
-- >>> (\x -> x * 2) 5
-- 2
-- 6
-- 10
--
-- with where clause
sumSquareOrSquareSum :: (Num p, Ord p) => p -> p -> p
sumSquareOrSquareSum x y = if sumSquare > squareSum
                            then sumSquare
                           else squareSum
        where sumSquare = x^2 + y^2
              squareSum = (x+y)^2

-- >>> sumSquareOrSquareSum 2 3
-- 25
--
-- Explicit agrument construction
sumSquareOrSquareSum1 :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum1 x y = if (x^2 + y^2) > ((x+y)^2)
                           then (x^2 + y^2)
                           else (x+y)^2
-- >>> sumSquareOrSquareSum1 2 3
-- 25
--

-- with secondary/supplementary function
body :: Ord p => p -> p -> p
body sumSquare squareSum = if sumSquare > squareSum
                           then sumSquare
                           else squareSum

sumSquareOrSquareSum2 :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum2 x y = body (x^2 + y^2) ((x+y)^2)
-- >>> sumSquareOrSquareSum2 2 3
-- 25
--
--secondary function in lambda
body1 :: Ord p => p -> p -> p
body1 = (\sumSquare squareSum ->
        if sumSquare > squareSum
        then sumSquare
        else squareSum)
-- Combine
sumSquareOrSquareSum3 :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum3 x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum
                              then sumSquare
                             else squareSum) (x^2 + y^2) ((x+y)^2)

-- >>> sumSquareOrSquareSum3 2 3
-- 25

--Compare sumSquareOrSquareSum3 and sumSquareOrSquareSum4
sumSquareOrSquareSum4 :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum4 x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum
                              then sumSquare
                             else squareSum) x y
-- >>> sumSquareOrSquareSum4 2 3
-- 3
--

-- let function in. (pg 28)
sumSquareOrSquareSum5 :: (Ord p, Num p) => p -> p -> p
sumSquareOrSquareSum5 x y = let sumSquare = (x^2 + y^2)
                                squareSum = ((x+y)^2)
                            in

                             if sumSquare > squareSum
                              then sumSquare
                             else squareSum
-- >>> sumSquareOrSquareSum5 2 3
-- 25
--

doubleDouble :: Num a => (a->b) -> a -> a
doubleDouble x =  (\x -> (x *2) * 2)

doubleDouble1 :: Num a => a -> a
doubleDouble1 x = (\dubs -> dubs*2) (x*2)
-- >>> doubleDouble1 2
--8
--
--lexicality means always looks for the nearest numbers for x and y
add3 y = (\y ->
            (\x -> y + x) 1 ) 3
-- >>> add3 1
-- 4
--
-- >>> add3 4
-- 4
--
-- >>> add3 8
-- 4
--

doubleCube :: Floating a => a -> a
doubleCube x = (\cube -> cube ** 3) (x * 2)
-- >>> doubleCube 2
-- 64.0

-- >>> (2*2)**3
-- 64.0

-- >>> doubleCube 5
-- 1000.0


-- Notice actually whatever x is is not relevant
doubleCube1 :: Floating a => p -> a
doubleCube1 x = (\cube -> cube ** 3) (4)

-- >>> doubleCube1 5
-- 64.0

-- >>> doubleCube1 7
-- 64.0

-- >>> doubleCube1 20
-- 64.0
