module Lib
    ( isPalindrome
    , preprocess
    , prop_punctuationInvariant
    ) where

import Data.Char (isPunctuation)

preprocess :: String -> String
preprocess text = filter (not . (`elem` ['!', '.'])) text

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
    where cleanText = preprocess text


--- not tested in Spec.hs but a way for property testing
prop_punctuationInvariant text = preprocess text ==
                                 preprocess noPuncText
    where noPuncText = filter (not. isPunctuation) text


--Quick check 36.4 Write a property prop_reverseInvariant that demonstrates the obvious fact that the results of isPalindrome should be the same whether or not you reverse the input.
prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)