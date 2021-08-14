-- Verion 0 (flawed)
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

-- >>> myTake 2 [1,2,3]
-- [1,2]
-- >>> myTake 4 [1,2,3]
-- Prelude.head: empty list

-- Version 0+ with pattern matching
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []   --- FIX this with myTakePM _ [] = [] Quick check 38.1
myTakePM n (x:xs) = x : myTakePM (n-1) xs


-- Quick check 38.2  The following are all partial functions included in Prelude. For what inputsdo they fail?
-- Solution: use their type signature and derive the error.
-- maximum :: Ord a => [a] -> a will fail on an empty list
-- succ :: Enum a => a -> a will fail on the maxbound for the type
-- sum :: (Foldable t, Num a) => t a -> a will fail on an infinite list

-- Version 1
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

--FUNCTOR STYLE
-- >>> (+2) <$> maybeHead [1]
-- Just 3
-- >>> (+2) <$> maybeHead []
-- Nothing

-- APPLICATIVE STYLE
-- >>> (:) <$> maybeHead [1,2,3] <*> Just []
-- Just [1]
-- >>> (:) <$> maybeHead [] <*> Just []
-- Nothing

-- Version 2 with Maybe and Monad that even accept error inputs
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

-- >>> myTakeSafer 3 (Just [1,2,3])
-- Just [1,2,3]
-- >>> myTakeSafer 6 (Just [1,2,3])
-- Nothing

-- Version 3 with Either. Left constructor returns a String, whereas the Right constructor returns the value from the first item in your list.
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]
intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"
charExampleEmpty :: [Char]
charExampleEmpty = ""

-- >>> eitherHead intExample
-- Right 1
-- >>> eitherHead intExampleEmpty
-- Left "There is no head because the list is empty"
-- >>> eitherHead charExample
-- Right 'c'
-- >>> eitherHead charExampleEmpty
-- Left "There is no head because the list is empty"

-- Either type is also a member of Monad (and thus Functor and Applicative as well) and can be applied with <$>
-- >>> (+ 1) <$> (eitherHead intExample)
-- Right 2
-- >>> (+ 1) <$> (eitherHead intExampleEmpty)
-- Left "There is no head because the list is empty"

-- Quick check 38.4 Use <$> and <*> to add the first and second numbers in intExample by using eitherHead.
-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- *Main> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- >>> tail intExample
-- [2,3]
-- >>> eitherHead (tail intExample)
-- Right 2

sum2e :: Either String Int
sum2e = (+) <$> (eitherHead intExample) <*> (eitherHead (tail intExample))

---------------
-- Examples with isPrime
-- Version 0
primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

isPrime :: Int -> Either String Bool
isPrime n
    | n < 2 = Left "Numbers less than 2 are not candidates for primes"
    | n > maxN = Left "Value exceeds limits of prime checker"
    | otherwise = Right (n `elem` primes)

-- >>> isPrime 5
-- Right True
-- >>> isPrime 6
-- Right False
-- >>> isPrime 100
-- Left "Value exceeds limits of prime checker"
-- >>> isPrime (-29)
-- Left "Numbers less than 2 are not candidates for primes"

-- Version 1. Using class to rep errors
-- 1. Create a type for error
data PrimeError = TooLarge | InvalidValue
-- 2. Make PrimeError an instance of Show so as to print errors.
instance Show PrimeError where
    show TooLarge     = "Value exceed max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"
-- 3. Refactor the isPrime function to show the errors so it looks neater now
isPrime1 :: Int -> Either PrimeError Bool
isPrime1 n
    | n < 2 = Left InvalidValue
    | n > maxN = Left TooLarge
    | otherwise = Right (n `elem` primes)
-- >>> isPrime1 0
-- Left Value is not a valid candidate for prime checking
-- >>> isPrime1 99
-- Left Value exceed max bound

-- 4. create a displayResult function that will convert your Either response into a String
displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError --- rem data PrimeError = TooLarge | InvalidValue


-- 5. create a IO 
main :: IO ()
main = do
    print "Enter a number to test for primality:"
    n <- read <$> getLine
    let result = isPrime1 n
    print (displayResult result)