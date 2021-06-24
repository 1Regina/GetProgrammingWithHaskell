{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

sel12 :: (a1, a2, a3, a4) -> a2
sel12 (a1, a2, a3, a4) = a2
manyItems :: (Int, Double, String, Char)
manyItems = (24,  0.99, "Haskell", 'H')

manyItemsString :: (Int, Double, [Char], Char)
manyItemsString = (24,  0.99, "Haskell", 'H')

-- >>> sel12 manyItems
-- 0.99

-- >>> sel12 manyItemsString
-- 0.99


-- QuickCheck 11.1
-- A function that returns only whole number after division using the `div` function
halve :: Integer -> Integer
halve n = n `div` 2  -- (div) 5 2

-- >>> halve 3
-- 1


half :: Int -> Double
half n = fromIntegral n / 2
-- >>> half 5
-- 2.5

-- QuickCheck 11.2
printDouble :: Int -> String
printDouble n = show (2*n)
-- >>> printDouble 4
-- "8"


-- Quickcheck 11.3
-- From start
-- makeAddress :: Int -> String -> String -> (Int,String,String)
-- Return type   (123,"Happy St","Haskell Town") :: (Int, String, String)

-- 1st argument 123
-- makeAddress 123 :: String -> String -> (Int,String,String)

-- -- 2nd argument Happy St
-- ((makeAddress 123 ) "Happy St") :: String -> (Int, String, String)

-- -- 3rd argument Haskell Town
-- (((makeAddress 123 ) "Happy St") "Haskell Town " ) :: (Int, String, String)


-- Quickcheck 11.4
-- map show [1,2,3,4] -- map must always return the sametype as it currently is. e.g applied fn to a [Int] returns a [Int]
-- 1 example for (a -> a) is show
-- >>> map show [1,2,3,4]
-- ["1","2","3","4"]
-- this proves map show [1,2,3,4] is not possible. Show gives a string instead of an int.
-- map isn’t about iteration, but about transforming a list of one type into a list of another type.
