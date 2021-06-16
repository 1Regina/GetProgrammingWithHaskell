ifEven :: Integral p => (p -> p) -> p -> p
ifEven f x = if even x
             then f x
             else x

genIfXEven :: Integral p => p -> (p -> p) -> p
genIfXEven x = (\f -> ifEven f x)


-- >>> ifEven (+1) 1
-- 1

