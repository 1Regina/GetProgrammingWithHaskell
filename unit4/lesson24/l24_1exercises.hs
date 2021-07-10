{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- Q24.1    Write a version of the Unix cp program that will copy a file and allow you to rename it (just mimic the basic functionality and don’t worry about specific flags).

-- only putStrLn without execuition of IO ouput
mainFail :: IO()
mainFail = do
        putStrLn "mv hello.txt hello2.txt"

-- readFile :: FilePath -> IO String
--  writeFile :: FilePath -> String -> IO ()

main :: IO ()
main = do
    args <- getArgs
    let source =  args !! 0
    let dest = args !! 1
    input <- TI.readFile source
    TI.writeFile dest input

-- Steps
-- 1. ghc --make l24_1exercises.hs
-- 2. ./l24_1exercises hello.txt hello2.txt
-- 3. (check that hello2.txt is generated)

