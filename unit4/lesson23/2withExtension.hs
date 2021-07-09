{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

import Data.Semigroup --to use <> and mconcat to join Text since there is no T.++

-- Common useful Text functions
-- 1. lines
sampleInput :: T.Text
sampleInput = "this\nis\ninput"
-- GHCi>T.lines sampleInput
-- ["this","is","input"]
-- GHCi> T.unlines (T.lines sampleInput)
-- "this\nis\ninput\n"
-- >>> T.lines sampleInput
-- ["this","is","input"]
-- >>> T.unlines(T.lines sampleInput)
-- "this\nis\ninput\n"

-- 2. words. handles white space. note that unwords dont get back the same thing
someText :: T.Text
someText = "Some\ntext for\t you"
-- GHCi> T.words someText
-- ["Some","text","for","you"]
-- GHCi> T.unwords (T.words someText)
--"Some text for you"
-- >>> T.words someText
-- ["Some","text","for","you"]
-- >>> T.unwords (T.words someText)
-- "Some text for you"




-- 3. splitOn opposite of intercalate (join)
breakText :: T.Text
breakText = "simple"
break2Text :: T.Text
break2Text = "simple to"
exampleText :: T.Text
exampleText = "This is simple to do"
-- GHCi> T.splitOn breakText exampleText
-- ["This is "," to do"]
-- >>> T.splitOn breakText exampleText
-- ["This is "," to do"]
-- >>> T.splitOn break2Text exampleText
-- ["This is "," do"]

-- GHCi> T.intercalate breakText (T.splitOn breakText exampleText)
-- "This is simple to do"
-- >>> T.intercalate breakText (T.splitOn breakText exampleText)
-- "This is simple to do"

-- >>> T.intercalate break2Text (T.splitOn break2Text exampleText)
-- "This is simple to do"

-- 4. concatenation
combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]
combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- >>> combinedTextMonoid
-- "some text"
-- >>> combinedTextSemigroup
-- "some text"


-- Quick check 23.3    Create    your    own    version    of    T.lines and T.unlines by using splitOn and T.intercalate.

-- breakSpace :: T.Text
-- breakSpace = " "
mylines :: T.Text -> [T.Text ]
mylines exampleText = T.splitOn "\n" exampleText
-- >>> mylines exampleText
-- ["This is simple to do"]

textLines = mylines exampleText

myUnlines :: [T.Text ] -> T.Text
myUnlines textLines = T.intercalate "\n" (textLines)

-- >>> myUnlines textLines 
-- "This is simple to do"

