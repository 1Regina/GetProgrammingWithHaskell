-- based from unit6/lesson34/2isPalindromeDraft.hs
-- -- METHOD A
module Main where
import qualified Data.Text as T
import Data.Text.IO as TIO ( getLine )
import qualified PalindromeText -- as Palin  (Alterantive short name)


-- isPalindrome :: T.Text -> Bool
-- isPalindrome text = text == T.reverse text -- this only works for racecar but not A man a plan a canal Panama

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palindrome!"
    text <- TIO.getLine
    let response = if PalindromeText.isPalindrome text  -- rely on the import isPalindrome fuunction from PalindromeText.hs in the same directory -- (with alternative short name)  = if Palin.isPalindromeText text

                    then "it is!"
                    else "it's not!"
    print response

-- METHOD B
-- module Main where
-- import qualified Data.Text as T
-- import Data.Text.IO as TIO ( getLine )
-- import qualified PalindromeText

-- main :: IO ()
-- main = do
--     print "Enter a word and I'll let you know if it's a palindrome!"
--     text <- TIO.getLine
--     let response = if PalindromeText.isPalindrome text
--                    then "it is!"
--                    else "it's not!"
--     print response


-- -- METHOD C
-- module Main where
-- import qualified Data.Text as T
-- import Data.Text.IO as TIO ( getLine )
-- -- import qualified PalindromeText

-- isPalindrome :: T.Text -> Bool
-- isPalindrome text = text == T.reverse text --  only for racecar

-- main :: IO ()
-- main = do
--     print "Enter a word and I'll let you know if it's a palindrome!"
--     text <- TIO.getLine
--     let response = if isPalindrome text
--                    then "it is!"
--                    else "it's not!"
--     print response

-- -- Steps:
-- --1.  ghc Main.hs
-- --2. ./Main
-- --3. use  only for racecar but not "A man a plan a canal Panama" BCOS it is isPalindrome only take reverse
