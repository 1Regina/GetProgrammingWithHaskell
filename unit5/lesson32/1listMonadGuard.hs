import Control.Monad -- to use guard to filter values in a list.

--  list as a context with Monad abstracting out the context of the list.
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 ..n]
    return (2^value)

-- *Main> powersOfTwo 10
-- [2,4,8,16,32,64,128,256,512,1024]


--  list as a list data structure, not abstracting out the context of the list.
powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1 .. n]

-- >>> powersOfTwoMap 10
-- [2,4,8,16,32,64,128,256,512,1024]

-- with fmap
powersOfTwoFmap :: Int -> [Int]
powersOfTwoFmap n = fmap (\x -> 2^x) [1 .. n]
-- >>> powersOfTwoFmap 10
-- [2,4,8,16,32,64,128,256,512,1024]

-- combine two lists easily. Suppose you want powers of 2 and 3 as n pairs
powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2^value  ---- [2,4,8,16,32]
    let powersOfThree = 3^value --  map (\x -> 3^x)  [1..5] = [3,9,27,81,243]
    return (powersOfTwo,powersOfThree) -- not the same  return ([2,4,8,16,32],  [3,9,27,81,243])
-- >>> powersOfTwoAndThree 5
-- [(2,3),(4,9),(8,27),(16,81),(32,243)]
-- >>> powersOfTwo 5
-- [2,4,8,16,32]
-- >>>  map (\x -> 3^x)  [1..5]
-- [3,9,27,81,243]
-- >>> powersOfTwo 5
-- [2,4,8,16,32]

--- powersOfTwoAndThree makes pairs as each element in list is created. whereas powersOfTwo3 takes 2 completed list and do map
powersOfTwo3 :: [(Int,Int)]
powersOfTwo3 = do
    value2 <- [2,4,8,16,32]
    value3 <- [3,9,27,81,243]
    return (value2, value3)
-- >>>powersOfTwo3
-- [(2,3),(2,9),(2,27),(2,81),(2,243),(4,3),(4,9),(4,27),(4,81),(4,243),(8,3),(8,9),(8,27),(8,81),(8,243),(16,3),(16,9),(16,27),(16,81),(16,243),(32,3),(32,9),(32,27),(32,81),(32,243)]

-- Because evenValue andoddValue were created with <-,this pair represents all possiblepairs from the two values like powersOfTwo3
allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)
-- >>> allEvenOdds 5
-- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]
-- >>> allEvenOdds 6
-- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]

-- Quick check 32.1    Use do-notation to generate pairs of numbers up to 10 and their squares.
squarePairs :: Int -> [(Int, Int)]
squarePairs n = do
    digit <- [1..n]
    -- squares <- [(1^2) .. (n^2)]
    return (digit, digit ^2) -- (digit, squares)

-- 32.1.1 The guard function
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value
-- >>> evensGuard 5
-- [2,4]

-- Quick check 32.2    Write filter by using guard and do-notation
-- filter :: (a -> Bool) -> [a] -> [a]
guardFilter ::  (a -> Bool) -> [a] -> [a]
guardFilter function aList = do
    val <- aList
    guard (function val)
    return val
