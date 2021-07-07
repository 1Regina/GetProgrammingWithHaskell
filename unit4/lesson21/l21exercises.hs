
-- Q21.1    Translate listing 21.1 (reproduced below) into code by using do-notation in a Maybe. Assume that all the user input is replaced with a Map with a value for the input. Ignore the first putStrLn and simply return the statement at the end.

import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

user :: Map.Map Int String
user = Map.fromList [(1, "HaskellGal")]

-- see types with Maps
-- *Main> :t Map.fromList
-- Map.fromList :: Ord k => [(k, a)] -> Map.Map k a
-- *Main> :t Map.lookup
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

maybeMain :: Maybe String
maybeMain = do
    -- putStrLn "Hello! What's your name?"
    -- name <- getLine
    name <- Map.lookup 1 user
    let statement = helloPerson name
    -- putStrLn statement
    return statement

-- >>> maybeMain
-- Just "Hello HaskellGal!"


-- Q21.2    Create a program that asks the user to input a number and then returns the nth Fibonacci numbers (see lesson 8 for an example of computing Fibonacci numbers).

fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n - 2)

mainFibo :: IO ()
mainFibo = do
        putStrLn  "Input the number"
        number <- getLine
        let answer = fib (read number) -- rem number is an IO int and I need it in Int. use read
        putStrLn (show answer)  -- answer is a Int but putStrLn needs a string
        -- print answer
