halve :: Integer -> Integer
halve = (\x -> x `div` 2)

-- >>> halve 10
-- 5
--

-- printDouble :: (Show a, Num a) => a -> String
printDouble :: Int -> String
printDouble x = show ( x * 2)

-- >>> printDouble 5
-- "10"
--

z :: Double
z = read "6"

-- >>> z
-- 6.0
--

p :: Double
p = z / 2  -- use this as a getaround to determine


