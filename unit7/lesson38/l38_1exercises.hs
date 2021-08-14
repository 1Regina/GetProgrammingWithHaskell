-- Q38.1
-- Make a function `addStrInts` that takes two `Ints` represented as `Strings` and adds them. The function would return an `Either String Int`. The Right constructor should return the result, provided that the two arguments can be parsed into Ints (use Data.Char isDigitto check). Return a different Left result for the three possible cases:
-- First value can’t be parsed.
-- Second value can’t be parsed.
-- Neither value can be parsed.

import Data.Char (isDigit)

-- Note that String is a list of [Char].
-- >>> ['h','e','l','l','o']
-- "hello"

--------------------------------------------------------------
-- this fails bcos first need to convert the string to char so isDigit can be applied as isDigit :: Char -> Bool
-- addStrInts' :: String -> String -> Either String Int
-- -- addStrInts Right int
-- addStrInts' s1 s2
--     | ((isDigit s1) == True)  && ((isDigit s2) == True)  = Right ((read s1) + (read s2))
--     | ((isDigit s1) == False) && ((isDigit s2) == True)  = Left "First value can’t be parsed."
--     | ((isDigit s1) == True)  && ((isDigit s2) == False) = Left "Second value can’t be parsed."
--     | otherwise = Left "Neither value can be parsed."



-- With String as [Char], use map to apply to char in the list

-- addStrInts' :: Char -> Char -> Either [Char] b  -- unfortunately requirements wants String input
-- addStrInts' s1 s2
--     | ((isDigit s1) == True)  && ((isDigit s2) == True)  = Right ((read s1) + (read s2))
--     | ((isDigit s1) == False) && ((isDigit s2) == True)  = Left "First value can’t be parsed."
--     | ((isDigit s1) == True)  && ((isDigit s2) == False) = Left "Second value can’t be parsed."
--     | otherwise = Left "Neither value can be parsed."

-- NA as qn requirement is otherwise return a string
-- Type AddError = FirstVal | SecondVal | NeitherVal
-- instance Show AddError where
--     show FirstVal   = "First value can’t be parsed."
--     show SecondVal  = "Second value can’t be parsed."
--     show NeitherVal = "Neither value can be parsed."

--------------------------------------------------------------

allDigits :: String -> Bool
allDigits val = all (== True) (map isDigit val)

addStrInts :: String -> String -> Either Int String
addStrInts val1 val2
    | allDigits val1 && allDigits val2 = Left (read val1 + read val2)
    | not (allDigits val1 || allDigits val2) = Right "both args invalid"
    | not (allDigits val1) = Right "first arg invalid"
    | otherwise = Right "second arg invalid"
