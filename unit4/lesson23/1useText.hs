import qualified Data.Text as T

-- -- Type signature for pack and unpack
-- T.pack :: String -> T.Text
-- T.unpack :: T.Text -> String


firstWord :: String
firstWord = "pessimism"
secondWord :: T.Text
secondWord = T.pack firstWord
thirdWord :: String
thirdWord = T.unpack secondWord

-- >>> thirdWord
-- "pessimism"

-- Quick check 23.1    Create    fourthWord once again, making the String type T.Text
fourthWord :: T.Text
fourthWord = T.pack thirdWord
-- >>> fourthWord
-- "pessimism"

-- Literal Strings cannot define Text. -- Error : Couldn't match expected type 'T.Text' with actual type '[Char]'
-- myWord :: T.Text
-- myWord = "dog"

myNum1 :: Int
myNum1 = 3
myNum2 :: Integer
myNum2 = 3
myNum3 :: Double
myNum3 = 3

-- Quick check 23.2    There’s a language extension called TemplateHaskell. How would you com-pile templates.hs to use this extension? How would you add it using a LANGUAGE pragma?
-- 2 otpions
-- $ghc text.hs -XTemplateHaskell
-- {-# LANGUAGE  TemplateHaskell #-}