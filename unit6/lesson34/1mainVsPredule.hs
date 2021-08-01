-- Quick check 34.1    Suppose you need to store the length of an object as a variable. For example:length :: Intlength = 8How would you use that value without conflicting with the existing length function in Prelude?
-- answer: qualify the value as Main.length

length :: Int
length = 8
doubleLength :: Int
doubleLength = Main.length * 2
-- Steps
-- 1. Prelude> :l 1mainVsPredule.hs
-- 2. *Main> Main.length
-- 3. *Main> Main.length *2


-- Quick check 34.2    Modify the module declaration so that you also export preprocess.
-- module Palindrome (isPalindrome, preprocess) where
-- Or
-- module Palindrome
--     ( isPalindrome
--     , preprocess
--     ) where

-- Quick check 34.3    You    shouldn’t    leave    Main.isPalindrome there, as it’s no longer necessary.If you remove the code for Main.isPalindrome, how can you refactor your code so you no longerneed to qualify Palindrome.isPalindrome?

-- METHOD B -- uncomment module n import n copy Method B to new main.hs
-- module Main where
-- import Palindrome ()

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palindrome!"
    text <- getLine
    let response = if isPalindrome text
                   then "it is!"
                   else "it's not!"
    print response

-- Steps:
--1.  ghc Main.hs
--2. ./Main