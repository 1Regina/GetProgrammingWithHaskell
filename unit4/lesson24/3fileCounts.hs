import System.IO
import System.Environment


-- Count file contents metadata. Progam that takes a file as an argument, and then count the characters, words, and lines in the file. The program will display this data to the user and append the info to a stats.dat file.

--1.  getCounts collects character, word, and line count info into a tuple
getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
    where charCount = length input
          wordCount = (length . words) input
          lineCount = (length . lines) input

--2. countsText function to convert a 3-tuple of counts into a human-readable summary. You’ll use unwords to join your text.Listing 24.5 Sample contents of stats.dat file for your fileCounts.hs programListing 24.6 getCounts collects character, word, and line count info into a tuple

countsText :: (Int,Int,Int) -> String
countsText (cc,wc,lc) =  unwords ["chars: "
                                 , show cc
                                 , " words: "
                                 , show wc
                                 , " lines: "
                                 ,  show lc]

-- >>> (countsText . getCounts) "this is\n some text"
-- "chars:  18  words:  4  lines:  2"

-- Quick check 24.3    Why is it preferable to use unwords instead of combining your strings with ++?
-- The ++ operator is specific to lists. In lesson 23, we talked at length about the other text types beyond String. The unwords function has a version for Text as well as String, whereas ++ works only on type String. Using unwords makes it much, much easier to refactor your code if you decideto swap out String for Text.


-- 3. A program that put all the metadata into a file but cannot  do ./3fileCounts stats.dat
mainMeta :: IO ()
mainMeta = do
       args <- getArgs   -- need import System.Environment
       let fileName = head args
       input <- readFile fileName
       let summary = (countsText . getCounts) input
       appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])
       putStrLn summary

-- Steps to run
-- 1. ghc --make 3fileCounts.hs
-- 2. ./3fileCounts hello.txt
-- 3. cat stats.dat


-- 3(other problem). Put all the metadata into a file and close it so the program can read the stats.dat file by closing thef file handle. It prevent the error involving appendFile from trying to write a file that you still have open but gets [illegal operation (delayed read on➥closed handle)]
-- root problem : lazy evaluation. In this code below
       -- 1. Your input isn’t used until you define summary but..
       -- 2. Summary isn’t used until you call appendFile
       -- 3. Because appendFile performs an IO action, it does force summary to be evaluated, which forces input to be evaluated.
       -- 4. real problem is that hClose closes the file immediately because it’s an IO action and must happen as soon as you evaluate it.
mainCloseTooSoon :: IO ()
mainCloseTooSoon = do
       args <- getArgs
       let fileName = head args
       file <- openFile fileName ReadMode
       input <- hGetContents file
       hClose file
       let summary = (countsText . getCounts) input
       appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])
       putStrLn summary

       -- 5. ... continue ..put hClose after appendFile because that’s when summary is finally evaluated,
       --   `appendFile (mconcat [fileName, " ",summary, "\n"])
       --    hClose file`
       -- 6.  back where you started; you’re closing the file after you need a new han-dle!

-- 3 (Solution for Lazy) needs to evaluate summary before you write to the file by move putStrLn summary before you write to the file. so it will
       -- i. force summary to be evaluated first.
       -- ii. Then close the handle,
       -- iii. finally appending the file.
main :: IO ()
main = do
       args <- getArgs
       let fileName = head args
       file <- openFile fileName ReadMode
       input <- hGetContents file
       let summary = (countsText . getCounts) input
       putStrLn summary
       hClose file
       appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])

-- Quick check 24.4 Why doesn’t readFile close the handle?
       -- readFile :: FilePath -> IO String
       --  readFile name = do
       --  inputFile <- openFile name ReadMode
       --  hGetContents inputFile
-- Because of lazy evaluation, if readFile closes the handle, you’d never be able to use the contents of the file. This is because a function acting on the contents of the file wouldn’t be called until after the file handle was closed
