-- Q11.1
-- filter :: (a -> Bool) -> [a] -> [a]
-- >>> filter even [2,4,5,6,7]
-- [2,4,6]
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- If you look at map, you can see there are two differences:
-- map :: (a -> b) -> [a] -> [b]
-- First is that the function passed into filter must return a Bool.Second is that map can transform the type of the list, whereas filter can’t

-- Filter apply a function that generate a bool when applied to a list and output only those elements in the list that meets (ie True) the bool function.

-- Map apply any function (^2 or +1 ) to a list and output the transformed values of every element of the list.

-- Q11.2
-- Prelude> :t head
-- head :: [a] -> a
-- Prelude> :t tail
-- tail :: [a] -> [a]


-- myHead :: [a] -> [ a]
-- myHead (x:_) = x
-- myHead [ ] = [ ]
-- >>> myHead [1,2,3]

myTail :: [a] -> [ a]
myTail (_:xs) = xs
myTail [ ] = [ ]
-- >>> myTail [1,2,3]-
-- [2,3]
--

-- Q11.3
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs  
    where newInit = f init x

-- >>> myFoldl (-) 0 [1,2,3,4] 
-- -10
--
