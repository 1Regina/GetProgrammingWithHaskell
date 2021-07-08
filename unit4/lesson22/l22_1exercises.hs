import System.Environment
import Control.Monad


-- -- Q22.1    Write a program, simple_calc.hs, that reads simple equations involving adding two numbers or multiplying two numbers. The program should solve the equation each user types into each line as each line is entered.
-- equation :: [String] -> Int
-- equation (arg1 :"+":arg2:theRest)= read arg1 + read arg2
-- equation (arg1 :"*":arg2:theRest)= read arg1 * read arg2


-- toInts :: String -> [Int]
-- toInts = map read . lines

-- main2 :: IO ()
-- main2 = do
--     putStrLn "Input addition or multiply equations"
--     userInput <- getContents       --- treat the I/O stream for STDIN as a list of characters.
--     let numbers = lines userInput  -- make the list of char into a a String
--     print (equation numbers)

-- --STEPS
-- -- 1.  ghc --make summary.hs
-- -- 2.  ./summary
-- -- 3.  <ctdl-d> to end and get results of computation

-- calc :: [String] -> Int
-- calc (val1:"+":val2:rest) = read val1 + read val2
-- calc (val1:"*":val2:rest) = read val1 * read val2

-- main :: IO ()
-- main = do
--     userInput <- getContents
--     let values = lines userInput
--     print (values)

-- sampleInput :: [String]
-- sampleInput = ["21","+","123"]

main :: IO ()
main = do
    putStrLn "Is this addition or multiplication"
    answer <- getContents
    if read answer == "addition"

          then do
              putStrLn "write your addition equation"
              calSum <- getContents
              read calSum
          else do
              putStrLn "write your multiplication equation"
              calProduct <- getContents
              read calProduct