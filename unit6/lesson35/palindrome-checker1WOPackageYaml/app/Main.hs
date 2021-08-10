--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Palindrome (isPalindrome)
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "it is"
                   else "it's not!"
    TIO.putStrLn response