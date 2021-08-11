import Lib
import Test.QuickCheck

-- after stack install quickcheck-instances ie can handle Data.Text
import Test.QuickCheck.Instances -- new!
import Data.Char(isPunctuation)
import Data.Text as T            -- new!
--------------------------------------

--no change on assert
assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

-- testing one by one example WITHOUT QuickCheck for property testing
-- main :: IO ()
-- main = do
--     putStrLn "Running tests..."
--     assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
--     assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
--     assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
--     assert (isPalindrome ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"  -- quick check 36.3 (correct)
--     assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"  -- rem the contrary case

--     putStrLn "done!"


main :: IO ( )
main = do
    -- quickCheck prop_punctuationInvariant -- test property with only 100 tests
    quickCheckWith stdArgs { maxSuccess = 1000}  prop_punctuationInvariant -- test property with 1000 tests
    quickCheckWith stdArgs { maxSuccess = 1000}  prop_reverseInvariant
    quickCheck prop_reverseInvariant -- QuickCheck exercise 36.5: Add a quickCheck test for the prop_reverseInvariant defined in the preceding exercise
    putStrLn "done!"

