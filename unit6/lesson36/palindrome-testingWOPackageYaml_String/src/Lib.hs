module Lib
    ( isPalindrome
    , preprocess
    , prop_punctuationInvariant
    , prop_reverseInvariant
    ) where

import Data.Char (isPunctuation) -- ORIGINAL
-- import Data.Text as T -- after stack install quickcheck-instances



-- ORIGINAL without stack install quickcheck-instances
preprocess :: String -> String
preprocess text = filter (not . isPunctuation) text -- filter (not . (`elem` ['!', '.', '[','\\'])) text ()  - slow one punctuation by 1 punctuation

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
    where cleanText = preprocess text

--- not tested in Spec.hs but a way for property testing
prop_punctuationInvariant text = preprocess text ==
                                 preprocess noPuncText
    where noPuncText = filter (not. isPunctuation) text


--Quick check 36.4 Write a property prop_reverseInvariant that demonstrates the obvious fact that the results of isPalindrome should be the same whether or not you reverse the input.
prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)
---------------------------------------------------------------
-- -- after stack install quickcheck-instances
-- preprocess :: T.Text -> T.Text
-- preprocess text = T.filter (not . isPunctuation) text -- filter (not . (`elem` ['!', '.', '[','\\'])) text ()  - slow one punctuation by 1 punctuation

-- isPalindrome :: T.Text -> Bool
-- isPalindrome text = cleanText == T.reverse cleanText
--     where cleanText = preprocess text


-- --- not tested in Spec.hs but a way for property testing
-- prop_punctuationInvariant text = preprocess text ==
--                                  preprocess noPuncText
--     where noPuncText = T.filter (not. isPunctuation) text


-- --Quick check 36.4 Write a property prop_reverseInvariant that demonstrates the obvious fact that the results of isPalindrome should be the same whether or not you reverse the input.
-- prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)