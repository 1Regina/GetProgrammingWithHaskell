import System.Environment
import Control.Monad
-- import Data.List.Split (splitOn) -- NA for without cabal


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
    answer <- getLine
    -- if answer == "addition"
    --       then do
    --           putStrLn "write your addition equation"
    --           calSum <- getLine
    --           return ()
    --       else do
    --           putStrLn "write your multiplication equation"
    --           calProduct <- getLine
    --           let p = read calProduct :: String
    --           return ()
    case answer of
        "addition" -> do
              putStrLn "write your addition equation"
              calSum <- getLine
              let [a,b] = split '+' calSum
              -- alt: use case of instead
              -- let ab = split '+' calSum
              -- case ab of [a,b] -> ...; _ -> error
              print $ read a + read b
        "multiplication" -> do
              putStrLn "write your multiplicat equation"
              calProd <- getLine
              let [a,b] = split '*' calProd
              -- return ()
              print $ read a * read b
        _ -> do
            putStrLn "pls select again"
            main

-- ddd+ddd


split :: Eq a => a -> [a] -> [[a]]
split needle [] = []
split needle (a : as)
   | needle == a = [] : split needle as
   | otherwise = case split needle as of
        [] -> [[a]]
        as' : ass -> (a: as') : ass



-- >>> split '+' "+68"
-- ["","68"]

-- >>> split '+' "+34+56"
-- "" : ["34","56"]

-- >>> split '+' "4+68"
-- ["4","68"]

-- >>> split '+' "34+68"
-- ["34","68"]


-- ["34","68"]
-- (a: as') : ass


-- needle = '+'
-- a = '3'
-- >>> split '+' "4+68"
-- ["4","68"]

-- "4":"68":[]
-- as' = "4"
-- ass = "68":[]


-- >>> split '+' ""
-- []


-- >>> split '+' "8"
-- ["8"]
-- [['8']]

-- >>> split '+' "68"
-- ["68"]
