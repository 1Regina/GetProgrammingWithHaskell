import System.IO

-- Steps to run program:
-- 1.  ghc --make filename.hs
-- 2.  ./filename
-- 3.  [DONT NEED] <ctdl-d> to end and get results of computation
-- 4. To run another program, rename the program to run to just `main`

 -- Below code that reads the first line from hello.txt and writes it to the console, and then reads the second line and writes it to a new file, goodbye.txt

-- read only 2 lines
main2lines :: IO ()
main2lines = do
        helloFile <- openFile "hello.txt" ReadMode
        firstLine <- hGetLine helloFile
        putStrLn firstLine
        secondLine <- hGetLine helloFile
        goodbyeFile <- openFile "goodbye.txt" WriteMode
        hPutStrLn goodbyeFile secondLine
        hClose helloFile
        hClose goodbyeFile
        putStrLn "done!"


-- read til end of file
mainEOF :: IO ()
mainEOF = do
    helloFile <- openFile "hello.txt" ReadMode
    hasLine <- hIsEOF helloFile
    firstLine <- if not hasLine
        then hGetLine helloFile
        else return "empty"
    putStrLn "done!"


-- Quick check 24.2    Write the code to check whether the second line is empty before writing itto a file
main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    hasSecondLine <- hIsEOF helloFile
    secondLine <- if not hasSecondLine
                then hGetLine helloFile
                else return ""
    putStrLn "done!"