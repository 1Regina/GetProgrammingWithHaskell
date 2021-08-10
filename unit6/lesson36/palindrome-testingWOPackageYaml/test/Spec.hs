import Lib
import Test.QuickCheck

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
    quickCheck prop_punctuationInvariant
    putStrLn "done!"