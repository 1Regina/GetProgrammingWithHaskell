-- Q8.1
myReverse :: [a] -> [a]
myReverse (x:xs) = (myReverse xs) ++ [x]
myReverse [ ] = [ ] 
-- myReverse (x:[]) = [x]

-- >>> myReverse [1..5]
-- [5,4,3,2,1]
-- >>> myReverse [3]
-- [3]
--

-- >>> 1 : 3 : []
-- [1,3]
--

-- Q8.2

fastFib :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter -1)

-- >>> fastFib 9 10 0
-- 0
-- >>> fastFib 9 10 1
-- 1
-- >>> fastFib 9 10 2
-- 1
-- >>> fastFib 9 10 3
-- 19
-- >>> fastFib 9 10 7
-- 122
-- >>> fastFib 19 9 6
-- 122
