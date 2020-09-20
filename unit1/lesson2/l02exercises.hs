--Q2.2
inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = 2 * x

square :: Num a => a -> a
square x = x^2
-- >>> square 2
-- 4
--

--Q2.3

check :: Integral a => a -> a
check n = if n `mod` 2 == 0
            then n - 2
          else 3 * n + 1  

-- >>> check 4
-- 2
--

check1 :: Integral a => a -> a
check1 n = if even n
           then n - 2
          else 3 * n + 1 
-- >>> check1 4
-- 2
--
