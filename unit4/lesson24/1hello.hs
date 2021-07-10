import System.IO


-- info about function argument
-- openFile :: FilePath -> IOMode -> IO Handle
-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath = String

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode



main :: IO ()
main = do
    myFile <- openFile "hello.txt" ReadMode
    hClose myFile
    putStrLn "done!"


-- Quick check 24.1    If you want to open a file named stuff.txt to read it, what will the function call look like?
-- openFile "stuff.txt" ReadMode
