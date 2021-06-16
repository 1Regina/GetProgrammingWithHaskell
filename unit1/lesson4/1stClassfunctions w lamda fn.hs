-- First class functions are functions with functions
-- >>> ifEven (\x -> x*2) 6
-- 12

ifEven myfunction x = if even x
                      then myfunction x
                      else x

cube  = (\x -> x ^ 3)

-- >>> cube 2

cubeIfEven x = ifEven cube x  -- same as cubeIfEven = ifEven cube
-- >>> cubeIfEven 2
-- 8

-- >>> ifEven cube 2
-- 8

-- >>> ifEven (\x -> x ^ 3) 4
-- 64
