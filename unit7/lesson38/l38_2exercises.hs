-- Q38.2    The following are all partial functions. Use the type specified to implement a safer version of the function:
-- 1. succ—Maybe
-- 2. tail—[a] (Keep the type the same.)
-- 3. last—Either (last fails on empty lists and infinite lists; use an upper bound for the infinite case.)

-- 1. succ :: Enum a => a -> a
safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc n
    | n == maxBound = Nothing
    | otherwise = Just (succ n)

-- >>> safeSucc 'a'
-- Just 'b'
-- >>> safeSucc '2'
-- Just '3'

-- alternative
safeSucc n = if n == maxBound
             then Nothing
             else Just (succ n)

-- 2. tail—[a] (Keep the type the same.) Note. tail :: [a] -> [a]
safeTail :: [a] -> [a]
safeTail (x :xs) = xs
safeTail [] = []

-- >>> safeTail ""
-- ""
-- >>> safeTail []
-- []
-- >>> safeTail "hello"
-- "ello"
-- >>> safeTail [1,2,3]
-- [2,3]
-- [2,3]

-- 3. last :: [a] -> a
-- safeLast :: [a] -> [a]
-- safeLast [] = []
-- safeLast (x:xs) = xs

safeLast :: [a] -> Either a String
safeLast [] = Right "empty list"
safeLast xs =  safeLast' 10000 xs
-- >>> safeLast [1,2]
-- Left 2

safeLast' :: Int -> [a] -> Either a String
safeLast' 0 _ = Right "List exceeds safe bound"
safeLast' _ (x:[]) = Left x
safeLast' n (x:xs) = safeLast' (n - 1) xs

-- >>> safeLast' 2 [2]
-- Left 2

-- >>> safeLast' 100001 [1,2,3,4,5]
-- Left 5
