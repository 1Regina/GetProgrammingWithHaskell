-- {-# LANGUAGE OverloadedStrings #-}
module Main where

import Compare
-- import qualified Data.Map as Map

main :: IO ()
main = do                                         -- to work with IO String by making it String
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <-  getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)           -- change IO 'Double' to IO '[Char]' for size &  cost
    let pizza2 = (read size2, read cost2)           -- change IO 'Double' to IO '[Char]' for size &  cost
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
    putStrLn "Alternatively, using my cheaperPizza function"
    putStrLn (cheaperPizza pizza1 pizza2)