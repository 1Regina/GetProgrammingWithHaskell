-- {-# LANGUAGE FlexibleContexts #-}
import System.Environment
import Control.Monad

-- import Data.List.Split

-- mainZ :: IO ()
-- mainZ = do
--     putStrLn "input integers"
--     args <- getArgs
--     mapM_ putStrLn args


-- getArgs :: IO [String]
-- getArgs args = IO (read args)

mainSum :: IO ()
mainSum = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    print linesToRead

-- Quick  check  22.1    Write  a  main  that  uses  mapM  to  call  getLine  three  times,  and  then  use mapM_ to print out the values’ input. (Hint: You’ll need to throw away an argument when using mapM with getLine; use (\_ -> ...) to achieve this.
-- io repeat function
quickcheck1 :: IO ()
quickcheck1 = do
        args <- mapM (\_ -> getLine) [1 .. 3]
        mapM_ putStrLn args



-- repeating reading number of lines per users arguments : replicateM . MUST import Control.Monad


mainReplicate :: IO ()
mainReplicate = do
            args <- getArgs
            let linesToRead = if length args > 0
                              then read (head args)
                              else 0 :: Int
            numbers <- replicateM linesToRead getLine
            let ints = map read numbers :: [Int]
            print (sum ints)


-- Quick check 22.2    Write    your    own    version    of    replicateM, myReplicateM, that uses mapM. (Don’tworry too much about the type signature.)
-- replicateM :: Applicative m => Int -> m a -> m [a]

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n function = mapM (\ _ ->  function ) [1 .. n]

--22.7
-- Lazy IO main
mainLazy :: IO ()
mainLazy = do
    userInput <- getContents
    mapM_ print userInput     -- print` function is `(putStrLn . show)`


-- Quick check 22.3    Use lazy I/O to write a program that reverses your input and prints it back to you.
mainLazyReverse :: IO ()
mainLazyReverse = do
    userInput <- getContents
    let reversee = reverse userInput
    putStr reversee


sampleData = ['6','2','\n','2','1','\n']


-- myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)