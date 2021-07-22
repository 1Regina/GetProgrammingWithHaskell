-- Task 1 : Monad to do transformation

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name ->
                return (nameStatement name)) >>=
            putStrLn

-- Rewriting helloName using do-notation
helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

-- Task 2: do to Monad transformation

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

--helloPersonDo
main' :: IO ()
main' = do
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

-- 2 ways to run
-- Method A
-- A1) ghci 1monad_do.hs
-- A2) main'
-- A3) [type name]

-- Method B -- need to rename main' to main as every program only run the main in the .hs file
-- B1) ghc 1monad_do.hs
-- B2) ./1monad_do
-- B3) [type name]

-- Task 2... rewrite do to Monad
main :: IO ()
main = getLine >>=
        (\name ->
            (\statement ->
                putStrLn statement) (helloPerson name))

-- 2 ways to run
-- Method A
-- A1) ghci 1monad_do.hs
-- A2) main
-- A3) [type name]

-- Method B -- name the program to run as main as every program only run the main in the .hs file
-- B1) ghc 1monad_do.hs
-- B2) ./1monad_do
-- B3) [type name]

-- Task 3 -- Rewrite echo by using do-notation
echoMonad :: IO ()
echoMonad = getLine >>= putStrLn

echoDo :: IO ()
echoDo = do
    someString <- getLine
    putStrLn someString
