module PalindromeText (isPalindrome, preprocess) where -- method 1
import qualified Data.Text as T
-- import qualified Data.Map as Map
import Data.Char (toLower,isSpace,isPunctuation)



stripWhiteSpace :: T.Text  -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text  -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

-- toLowerCase :: String -> String
-- toLowerCase text = map toLower text

preprocess :: T.Text  -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome ::  T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
        where cleanText = preprocess text

-- >>> isPalindrome (T.pack "racecar")
-- True

-- >>> isPalindrome (T.pack "A man a plan a canal Panama")
-- True
