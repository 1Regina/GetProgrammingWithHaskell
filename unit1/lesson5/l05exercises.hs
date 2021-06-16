-- Q5.1

ifEven :: Integral p => (p -> p) -> p -> p
ifEven function x = if even x
                    then function x
                    else x

inc :: Num a => a -> a
inc n = n + 1
double :: Num a => a -> a
double n = n*2

square :: Num a => a -> a
square n = n^2

ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven inc
ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven double
ifEvenSquare :: Integer -> Integer
ifEvenSquare = ifEven square

-- >>> ifEvenInc 3
-- >>> ifEvenDouble 3
-- >>> ifEvenSquare 3
-- 3
-- 3
-- 3

-- >>> ifEvenInc 4
-- >>> ifEvenDouble 4
-- >>> ifEvenSquare 4
-- 5
-- 8
-- 16


-- Q5.2
binaryPartialApplication :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
binaryPartialApplication binaryFunction arg = (\y -> binaryFunction arg y)

-- >>> binaryPartialApplication (-) 4 2
-- 2
--
