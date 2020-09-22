--Q6.1
repeat n = cycle [n]
--Q6.2
subseq :: Int -> Int -> [a] -> [a]
subseq start end list = drop start (take end list)
-- >>> subseq 2 5 [1 .. 10]
-- [3,4,5]
-- >>> subseq 2 7 "a puppy"
-- "puppy"
--

-- Q6.3
inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x list = elem x (take ((length list) `div` 2) list)

-- >>>  ( length "happy1" ) `div` 2 
-- 3
-- >>> inFirstHalf 's' "Cheese"
-- False
-- >>> inFirstHalf 'h' "three"
-- True
-- >>> inFirstHalf 'r' "three"
-- False
--
-- >>> inFirstHalf 13 [0,13 .. 100]
-- True
--
inFirstHalf1 :: Eq a => a -> [a] -> Bool
inFirstHalf1 x list = elem x firstHalf 
  where midpoint = (length list) `div` 2
        firstHalf = take midpoint list


-- >>> inFirstHalf1 's' "Cheese"
-- False
--
-- >>> inFirstHalf1 'h' "three"
-- True
--
-- >>> inFirstHalf1 'r' "three"
-- False
--
-- >>> inFirstHalf1 13 [0,13 .. 100]
-- True
--
