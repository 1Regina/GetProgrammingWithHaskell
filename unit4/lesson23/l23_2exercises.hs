-- Q23.2    Use    Data.Text.Lazy and Data.Text.Lazy.IO to rewrite the lazy I/O section from lesson 22 by using the Text type

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

--recall <1useText.hs>
-- firstWord :: String
-- firstWord = "pessimism"
-- secondWord :: T.Text
-- secondWord = T.pack firstWord
-- thirdWord :: String
-- thirdWord = T.unpack secondWord

-- recall <2withExtension.hs>
-- sampleInput :: T.Text
-- sampleInput = "this\nis\ninput"
-- >>> T.lines sampleInput
-- ["this","is","input"]

toInts :: T.Text -> [Int]
toInts = map (read. T.unpack) . T.lines
-- T.lines returns a list of strings in text form
-- T.unpack returns a string from a text

-- T.pack transforms a String into a text.


main :: IO ()
main = do
    userInput <- TIO.getContents
    let numbers = toInts userInput
    TIO.putStrLn ((T.pack . show . sum) numbers)

-- Steps
--         1.  ghc --make l23_2exercises.hs
--         2.  ./l23_2exercises
--         3.  [DONT NEED] <ctdl-d> to end and get results of computation