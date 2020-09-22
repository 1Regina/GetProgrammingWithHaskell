myLength :: Num p => [a] -> p
myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myLength1 :: Num a1 => [a2] -> a1
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs
-- >>> myLength1 [1,2,3,4]
-- 4
--
myTake :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest  
    where rest = myTake (n - 1) xs

-- >>> myTake 3 [1..10]
-- [1,2,3]
-- >>> myTake 3 [5..10]
-- [5,6,7]
-- >>> 1:3 : 2: []
-- [1,3,2]

-- >>> myTake 5 [11..20]
-- >>> myTake 4 [11..20]
-- >>> myTake 3 [11..20]
-- >>> myTake 2 [11..20]
-- >>> myTake 1 [11..20]
-- >>> myTake 0 [11..20]
-- [11,12,13,14,15]
-- [11,12,13,14]
-- [11,12,13]
-- [11,12]
-- [11]
-- []

-- myTake (n-1) xs = myTake (n-1) tail list
-- >>> myTake 4 [12..20]
-- [12,13,14,15]
-- >>> 11 : [12,13,14,15]
-- [11,12,13,14,15]
-- >>> myTake 4 $ tail [11..20]
-- [12,13,14,15]
-- >>> myTake 4 (tail [11..20])
-- [12,13,14,15]
--

finiteCycle :: [a] -> [a]
finiteCycle [ ] = [ ]
finiteCycle (first:rest) = first:rest ++ [first]
-- >>> finiteCycle [11,12,13,14,15]
-- [11,12,13,14,15,11]
--

myCycle :: [a] -> [a]
myCycle [ ] = [ ]
myCycle (first:rest) = first:myCycle (rest++[first])


-- >>> myCycle [11,12,13,14,15]
-- <command line>: unknown package: main
-- >>> 1!
-- >>> 1 + 3
-- 4
-- <BLANKLINE>
-- <interactive>:47:4: error:
--     parse error (possibly incorrect indentation or mismatched brackets)
--


