module Lib
    ( isPalindrome
    , preprocess
    , prop_punctuationInvariant
    , prop_spaceInvariant
    , prop_caseInvariant
    , prop_reverseInvariant,
    ) where

-- import Data.Char (isPunctuation) -- Added to use isPunctuation to cover all punctuations

import Data.Char (toLower,isSpace,isPunctuation)
import Data.Text as T -- after stack install quickcheck-instances


-- from lesson 35 in unit 6
stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.toLower text

---------------------------------------------------------------
-- after stack install quickcheck-instances
-- 1. punctuation property test
preprocessPunc :: T.Text -> T.Text
preprocessPunc text = T.filter (not . isPunctuation) text -- filter (not . (`elem` ['!', '.', '[','\\'])) text ()  - slow one punctuation by 1 punctuation
-- preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

-- 2. whiteSpace property test
preprocessSpace :: T.Text -> T.Text
preprocessSpace text = T.filter (not . isSpace) text

-- 3. lowerCase property test
preprocessCase :: T.Text -> T.Text
preprocessCase text = T.toLower text

-- 4. Mother property test
preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text


--- not tested in Spec.hs but a way for property testing
-- 1. punctuation property test
prop_punctuationInvariant text = preprocessPunc text == noPuncText
                                -- preprocessPunc noPuncText
    where noPuncText = T.filter (not. isPunctuation) text

-- 2. whiteSpace property test
prop_spaceInvariant text = preprocessSpace text == noSpaceText
                          -- preprocessSpace noSpaceText
    where noSpaceText = T.filter (not. isSpace) text

-- 3. lowerCase property test
prop_caseInvariant text = preprocessCase text == caseText
                         -- preprocessCase caseText
    where caseText = T.toLower text

--Quick check 36.4 Write a property prop_reverseInvariant that demonstrates the obvious fact that the results of isPalindrome should be the same whether or not you reverse the input.
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)