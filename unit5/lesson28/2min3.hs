-- minimum of three values.
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3) -- note the partial application here

-- create a simple IO action, readInt, which will read an Int from the command line
readInt :: IO Int
readInt = read <$> getLine

-- use <$> with <*> to make an IO action that reads in three Ints and returns the minimum
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")

-- Steps to run
-- 1. ghc 2min3.hs
-- 2. ./2min3.hs

-- Quick check 28.4    Use    minOfThree to get the Maybe Int value of these three Maybe values:Just 10Just 3Just
minOfThreeMaybe :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
minOfThreeMaybe val1 val2 val3 = min val1 (min val2 val3)

-- >>> minOfThree <$> Just 10 <*> Just 3 <*> Just 6
-- Just 3
