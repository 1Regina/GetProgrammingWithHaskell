-- based from unit6/lesson34/2isPalindromeDraft.hs
-- METHOD A
module Main where
import Palindrome
-- import qualified Palindrome as Palin --  (Alterantive short name)


-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palindrome!"
    text <- getLine
    let response = if Palindrome.isPalindrome text  -- rely on the import isPalindrome fuunction from Palindrome.hs in the same directory -- (with alternative short name)  = if Palin.isPalindrome text

                    then "it is!"
                    else "it's not!"
    print response

-- METHOD B
-- module Main where
-- import Palindrome ()

-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

-- main :: IO ()
-- main = do
--     print "Enter a word and I'll let you know if it's a palindrome!"
--     text <- getLine
--     let response = if isPalindrome text
--                    then "it is!"
--                    else "it's not!"
--     print response

-- Steps:
--1.  ghc Main.hs
--2. ./Main