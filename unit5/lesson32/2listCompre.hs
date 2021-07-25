import Control.Monad ( guard ) -- to use guard to filter values in a list.
import Data.Char (toUpper)
-- ist comprehension is doing just by looking at the beginning of it
allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = [(evenValue,oddValue) |  evenValue <- [2,4 ..]
                                      ,  oddValue <- [1,3 .. n]]

-- recall -- 32.1.1 The guard function
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)

    return value

-- list compre form
evensGuardCompre :: Int -> [Int]
evensGuardCompre n = [ value | value <- [1 .. n]
                             , even value]


-- Quick check 32.3    Write a list comprehension that takes the following words
-- ["brown","blue","pink","orange"] and
-- capitalizes the first letter, and prepends Mr. in front. (Hint: use Data.Char’s toUpper.)
mrCap :: [String ]
mrCap = [ "Mr" ++ capWord | strin <- ["brown","blue","pink","orange"]
                          , let capWord = (\(x : xs) -> (toUpper x : xs) strin ]



