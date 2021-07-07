import System.Random

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

-- Quick check  21.1
-- getLine retrieves the user's input. The input is a String


minDie :: Int
minDie = 1
maxDie :: Int
maxDie = 6
main1 :: IO ()
main1 = do
    dieRoll <- randomRIO (minDie,maxDie)
    putStrLn (show dieRoll)

-- Quick check 21.2    Is it okay if the last line in your main is getLine?
--  No, because the type of main is IO (), but the type of getLine is IO String
-- run in command
-- *Main> :t main1
-- main1 :: IO ()
-- *Main> :t getLine
-- getLine :: IO String


-- Quick check 21.3    Could you simplify your code to combine helloPerson and getLine like this?
-- `let statement = helloPerson getLin`
-- Ans: No, because getLine is still of type IO String so I still need `do`