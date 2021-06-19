-- 9.2
myMap :: (t -> a) -> [t] -> [a]
myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs

-- >>> myMap ("a " ++) ["car", "boat", "plane"]
-- ["a car","a boat","a plane"]
--


-- Quick check 9.1
myRemove :: (a -> Bool) -> [a] -> [a]
myRemove test [] = []
myRemove test (x : xs) = if test x
                         then myRemove test xs
                         else  x: myRemove test xs

-- >>> myRemove even [1,2,4,5,7,9,8]
-- [1,5,7,9]
--

-- >>> foldl (+) 10 []
-- 10
--



myProduct :: (Foldable t, Num b) => t b -> b
myProduct alist = foldl (*) 1 alist

-- >>> :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--


concatAll :: Foldable t => t [Char] -> [Char]
concatAll xs = foldl (++) "" xs
-- >>> concatAll ["aa","bb", "cc"]
-- "aabbcc"
--
-- >>> foldl (+) 0 [1,2,3,4]
-- >>> foldl (+) 1 [2,3,4]
-- >>> foldl (+) 3 [3,4]
-- >>> foldl (+) 6 [4]
-- >>> foldl (+) 10 []
-- 10
-- 10
-- 10
-- 10
-- 10
--
-- >>> foldl (-) 0 [1,2,3,4]  = ((((0 - 1) - 2) - 3) - 4)
-- >>> foldl (-) (-1) [2,3,4] = ((((- 1) - 2) - 3) - 4)
-- >>> foldl (-) (-3) [3,4]   = (((- 3) - 3) - 4)
-- >>> foldl (-) (-6) [4]     = -6 - 4
-- >>> foldl () (-10) []      = -10 - [] -- agar pattern
-- -10
-- -10
-- -10
-- -10

-- >>> foldr (-) 0 [1,2,3,4] -- = (1 - (2 - (3 - (4 - 0))))
-- >>> foldr (-) 4 [1,2,3]   -- = (1 - (2 - (3 - 4 )))
-- >>> foldr (-) (-1) [1,2]  -- = (1 - (2 - (-1) )
-- >>> foldr (-) (3) [1]     -- = (1 -3)
-- >>> foldr (-) (-2) []     -- = (-2 - 0) -- agar pattern
-- -2
-- -2
-- -2
-- -2
-- -2


-- 9.4 Using foldl for reverse
-- myReverse xs = foldl rcons [] xs
-- >>> foldl rcons [] [1,2,3]
-- >>> foldl rcons [1] [2,3]
-- >>> foldl rcons [2,1] [3]
-- >>> foldl rcons [3,2,1] []
-- [3,2,1]
-- [3,2,1]
-- [3,2,1]
-- [3,2,1]


rcons x y = y:x
myReverse xs = foldl rcons [] xs
-- >>> myReverse "Happy"
-- "yppaH"

-- >>> myReverse ["First", "of", "all"]
-- ["all","of","First"]

-- >>> myReverse [1,2,3]
-- [3,2,1]
