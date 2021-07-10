{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI


-- Q24.2    Write a program called capitalize.hs that will take a file as an argument, read that file, and then rewrite it capitalized.


capitalize :: T.Text -> T.Text
capitalize filecontents = T.toUpper filecontents

main :: IO ()
main = do
      args <- getArgs
      let fileName = head args
      input <- TI.readFile fileName
    --   TI.writeFile fileName (capitalize input)  -- not necessary
      TI.writeFile fileName (T.toUpper input)

-- Steps
-- 1. ghc --make l24_2exercises.hs
-- 2. ./l24_2exercises hello2.txt
-- 3. (check that hello2.txt is now capitalised)

