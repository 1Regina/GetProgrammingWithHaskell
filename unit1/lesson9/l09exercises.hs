
import Data.Char
-- Q9.1
myElem :: Eq a => a -> [a] -> Bool
myElem val myList = (length filteredList) /= 0  
  where filteredList = filter (== val) myList

-- >>> myElem 'a' "papaya"  
-- True
-- >>> myElem 1 [1,2,3,4]  
-- True
--
myElem1 :: Eq a => a -> [a] -> [a]
myElem1 val mylist = filter (== val) mylist
-- >>> myElem1 'a' "papaya"  
-- "aaa"
--
-- >>> myElem1 1 [1,2,3,4]  
-- [1]
--

-- Q9.2
-- >>> import Data.Char

-- >>> text =  "A man a plan a canal Panama"
-- >>> noSpaces = filter (/= ' ') text 
-- >>> noSpaces 
-- "AmanaplanacanalPanama"
--
-- >>> import Data.Char
-- >>>  map toLower "AmanaplanacanalPanama"
-- "amanaplanacanalpanama"
--
-- >>> "amanaplanacanalpanama" == reverse "amanaplanacanalpanama"
-- True
--

-- import Data.Char
isPalindrome :: [Char] -> Bool
isPalindrome text = processedPhrase == reverse processedPhrase
    where noSpace = filter (/=' ') text
          processedPhrase = map toLower noSpace

-- >>> isPalindrome " My Cat is Big "
-- False
-- >>> isPalindrome "A man a plan a canal Panama"
-- True
--
isPalindrome1 :: [Char] -> Bool
isPalindrome1 text = processedPhrase == reverse processedPhrase
    where processedPhrase = map toLower noSpace
          noSpace = filter (/=' ') text
          
-- >>> isPalindrome1 "A man a plan a canal Panama"
-- True
--

-- Q9.3 -- not ready
harmonic :: (Enum a, Fractional a) => Int -> a
harmonic n = sum (take n seriesValues)  
    where seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]        
          seriesValues = map   (\pair -> (fst pair)/(snd pair))   seriesPairs
-- >>> harmonic 4
-- 2.083333333333333
--
-- >>> seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]  
-- >>> take 5 seriesPairs
-- [(1.0,1.0),(1.0,2.0),(1.0,3.0),(1.0,4.0),(1.0,5.0)]
--

-- incorrect
harmonic1 :: (Eq p, Fractional p) => p -> p
harmonic1 n = 
    if (n == 1) 
    then 1
    else (1.00 / n) + (1.00 / harmonic1 (n - 1))

-- >>> harmonic1 4
-- 1.25
--


