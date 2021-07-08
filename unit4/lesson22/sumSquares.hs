import System.Environment
import Control.Monad


toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    putStrLn "Input integers to square and then sum"
    userInput <- getContents       --- treat the I/O stream for STDIN as a list of characters.
    let numbers = toInts userInput -- make the list of char ie String into a list of Int
    let squares = map (^2) numbers
    print (sum squares)

--STEPS
-- 1.  ghc --make summary.hs
-- 2.  ./summary
-- 3.  <ctdl-d> to end and get results of computation