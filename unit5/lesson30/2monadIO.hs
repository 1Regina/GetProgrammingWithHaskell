import System.Environment
echo :: IO ()
echo = getLine >>= putStrLn
-- main :: IO ()
-- main = echo

-- Steps to run
-- 1. ghc 2monadIO.hs
-- 2. ./2monadIO


-- Quick check 30.3    Combine readInt and printDouble (defined next) into a single IO action:
-- need (>>=) :: m a -> (a -> m b) -> m b

readInt :: IO Int
readInt = read <$> getLine
printDouble :: Int -> IO ()
printDouble n = print (n*2)

-- fmap's (<$>) :: Functor f => (a -> b) -> f a -> f b
readThenPrintDoubleAltFmap =   printDouble <$> readInt -- wrong type (IO (IO ()))

-- app's (<*>) :: Applicative f => f (a -> b) -> f a -> f b
readThenPrintDoubleApp =  pure printDouble <*> readInt  -- wrong type (IO (IO ()))

-- Monad's (>>=) :: m a -> (a -> m b) -> m b
readThenPrintDoubleMonad = readInt >>= printDouble -- CORRECT type IO ()

-- program to echo a string
echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String an we'll echo it!" >>
              getLine >>= putStrLn
-- main :: IO ()
-- main = echoVerbose

-- A. Task to print Hello name program
-- A1. The first is an IO action that will ask for the name; this is simply putStrLn with your question
askForName :: IO ()
askForName = putStrLn "What is your name?"

-- A2. next IO action you need to use is getLine. After that, you need to take the result of get-Line and make your "Hello, <NAME>!" string. This function is a regular function of the form String -> String
nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- end the results of this to putStrLn, and your action is finished. You start with chaining together askForName and getLine with >> because you don’t need the results:(askForName >> getLine)
-- With an IO String, but you need to connect it with name-Statement, which is a regular String -> String function. You can use >>= to do this if you can make nameStatement return an IO String. You could rewrite nameStatement, but a more com-mon solution is to wrap nameStatement in a lambda and use return at the end.

-- (askForName >> getLine) >>= (\name -> return (nameStatement name))
-- A3. hello program using 3 Monad methods
helloName :: IO ()
helloName = askForName >>
            getLine >>= (\name ->return (nameStatement name))>>=
            putStrLn
-- Steps to run
-- 1. ghci 2monadIO.hs or Prelude> :l 2monadIO.hs 
-- 2. helloName


-- Quick check 30.4    Turn (+ 2) from type Num a => a -> a to type Num a => a -> IO a using alambda and return. Use :t in GHCi to double-check that you’re getting the correct type.
-- returnInIO :: Integer -> IO Int
-- returnInIO = (\n -> return ((+ 2) n))

