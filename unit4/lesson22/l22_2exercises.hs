
import System.Environment
import Control.Monad

quotes :: [String]
quotes = ["quote 1"
         ,"quote 2"
         ,"quote 3"
         ,"quote 4"
         ,"quote 5"]


lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
    where quote = quotes !! (read x - 1)


main :: IO ()
main = do
    putStrLn "Input a number 1 to 5. Repeat after result is shown"
    userInput <- getContents
    mapM_ putStrLn (lookupQuote  (lines userInput))


--STEPS
-- 1.  ghc --make l22_2exercises.hs
-- 2.  ./l22_2exercises
-- 3.  <ctdl-d> to end and get results of computation