17. Ch21.0 Hello World! - Introducing IO types
    1. In an IO function: `getLine` returns the user input (which is an IO String)
        ```
        helloPerson :: String -> String
        helloPerson name = "Hello" ++ " " ++ name ++ "!"

        main :: IO ()
        main = do
            putStrLn "Hello! What's your name?"
            name <- getLine
            let statement = helloPerson name
            putStrLn statement
        ```
    2. Both `Maybe` and `IO` are parameterized types ie take another type as an argument.
        ```
        GHCi> :kind Maybe
        Maybe :: * -> *
        GHCi> :kind IO
        IO :: * -> *
        ```
    3. The context for the IO type is that the value has come from an input/output operation. ie will change state accordingly
    4. The context for the `Maybe` type is sometimes the program values might not be existent.
    5. IO are not functions. It is a tuple of 0 elements.  IO () is just IO parameterized with ().
    6. `Maybe` has two values, Just () and Nothing. `Just ()` is `Nothing`!
    7. `IO ()` is ~ `Just ()` is ~ `Nothing`.
    8. `main` needs a type so for nothing cases, use `IO()`
    9. `main` is not a function bcos it does not return a value. `main` is an `IO` action.
    10. An `IO` action is like a function but violate one of the 3 rules
        1.  All functions must take a value
        2.  All functions must return a value
        3.  Referential transparency - Anytime the same argument is supplied, the same value must be returned
    11. Like `main`, `putStrLn` is an IO action. `putStrLn` takes an argument and returns no value Rule #2
        ```
        putStrLn :: String -> IO ()
        ```
    12.  `getLine` takes no value but returns a type `IO String`, violates rule#1.   Because getLine violates this, it’s also an IO action
    13. randomRIO to generate a random number in a range. **import System.Random**.
        ```
        import System.Random
        minDie :: Int
        minDie = 1
        maxDie :: Int
        maxDie = 6
        main :: IO ()
        main = do
            dieRoll <- randomRIO (minDie,maxDie)
            putStrLn (show dieRoll)
        ```
    14. ** stack quick solve**`stack update stack install random` to resolve import System.Random could not find issue. This create an environment in the same folder with .hs file to run
    15.  randomRIO takes (the min/max pair) and returns an argument (an IO type parameterized with the type of the pair).  a different result, even with the same argument violate rule #3 so it is an action.
    16.  randomRIO, just like getLine and putStrLn, is an IO action
    17.  `getLine` return type `IO String` meaning a type from I/O and has to remain in context of IO type. e.g unit4/lesson21/io.hs
         1. random number using randomRIO, cant be used outside main or a similar IO action.
         2. Maybe: could take it out of context with filter isJust and isNothing
         3. a data from IO context stays in the IO context.
    18. recall `let` statements whenever you create variables that aren’t IO types.
    19. **do** bcos IO output can remain only in IO,`do` allows computations within IO context.
    20. Variables assigned with <- allow you to act as though a type IO a is just of type a. (to achieve let). In pt 1:
        1.  `getLine` returns `IO String`.
        2.  `name` :: IO String
        3.  BUT `helloPerson :: String -> String` and note String vs IO String
        4.  with `do`, using `<-` to assign IO String so it acts like String to pass to functions for reg Strings
        ```
        name <- getLine
        let statement = helloPerson name

        -- 1. `getLine` returns a type IO string
        -- 2. `helloPerson` takes a String type, not a IO String.
        -- 3. With assignment of `name` by using `<-` , you can treat itlike a normal String
        ```
    21. `do` help normal functions work with IO type
    22. `do` explanation : ![Alt text](unit4/lesson21/do_notation.png?raw=true "Why do is needed")
    23. **read** getLine returns an IO String, but you need your values to be of type Double. To solve this, you can use `read`.
    24. Compare unit4/lesson21/pizza.hs cheaperPizza in ghci vs main with do for IO context.
        1.  With main, we can make it more interactive.

    25.  IO can use do-notation because it’s a member of a powerful type class called Monad.
    26.  `do` can be used by any member of `Monad` to perform computation in a context.
    27.  Maybe is also a member of the Monad type class (ie instance Monad Maybe where ...) and therefore can also use do-notation.
    28.  With Maybe Monad which can implement `do` , implement maybeMain in case there is Nothing with
        1.   Map.fromList for a Map (aka Dictionary) for cost and size
            ```
            costData :: Map.Map Int Double
            costData = Map.fromList [(1,18.0),(2,16.0)]

            sizeData :: Map.Map Int Double
            sizeData = Map.fromList [(1,20.0),(2,15.0)]

            ```
        2. Map.lookup to return value for the key in Map.
            ```
            maybeMain :: Maybe String
            maybeMain = do
                size1 <- Map.lookup 1 sizeData
                cost1 <- Map.lookup 1 costData
                size2 <- Map.lookup 2 sizeData
                cost2 <- Map.lookup 2 costData
                let pizza1 = (size1,cost1)
                let pizza2 = (size2,cost2)
                let betterPizza = comparePizzas pizza1 pizza2
                return  (describePizza betterPizza)


                - *Main> :t Map.lookup
                -- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
                -- `return` :  a String is returned as a Maybe String so put it back in the do-context
                ouput:

                *Main> maybeMain
                Just "The 20.0 pizza is cheaper at 5.729577951308232e-2 per square inch."
            ```
    29.  `return` function, which takes a value of a type and puts it back in the context of the do-notation. In pt 28.2: a String is returned as a Maybe String
    30.  if Maybe Type instead of IO () for main type, consider Map.lookup for a Map.
    31.  To transform/covert from IO context to feed arg to functions, use these on the agr:
         1.   read    (to change IO type to non-IO type)
         2.   show    (to make into a String)
         3.   return  (to put output back in IO context)
18. Ch22.0: Interacting with command line and lazy I/O
    1.  ` getArgs` function in **System.Environment**. e.g unit4/lesson22/sum.hs
        1.  unknown how many args user will input
        2.  to sum agr from user input.
        3.  get a list of Strings in the context of IO. Cover the case of a user failing to enter an argument
            ```
            (not working well)
            main :: IO ()
            main = do
                args <- getArgs
                let linesToRead = if length args > 0
                                then read (head args)
                                else 0 :: Int
                print linesToRead
            ```
        4.  `mapM` (the M stands for Monad). It map over a list of values in the context of IO (technically, on any member of the Monad type class). It returns a list.
        5.  `mapM_` : works like mapM BUT throws away the result. Typically, when a function ends with an underscore in Haskell, it indicates that you’re throwing away the results
        6.  Rem to cover case where user didnt enter argument ie 0 lines.
        7.  `print` function is `(putStrLn . show)`
    2. IO repeat function: quickcheck 22.1: Write  a  main  that  uses  mapM  to  call  getLine  three  times,  and  then  use mapM_ to print out the values’ input. (Hint: You’ll need to throw away an argument when using mapM with getLine; use (\_ -> ...) to achieve this.
        ```
        quickcheck1 :: IO ()
        quickcheck1 = do
                args <- mapM (\_ -> getLine) [1 .. 3]
                mapM_ putStrLn args

        ```
    3. `replicateM` works with `getLine` . It is a function for iterating.  `replicateM`takes a value for the number of times you want to repeat and an IO action and repeats the action as expected. **import Control.Monad**
        ```
        mainReplicate :: IO ()
        mainReplicate = do
                    args <- getArgs   -- need to import System.Environment
                    let linesToRead = if length args > 0
                                    then read (head args)
                                    else 0
                    numbers <- replicateM linesToRead getLine
                    print "sum goes here"
        ```
        1. recall getLine returns a String in the IO contex.
        2. need to convert them to Ints, and then you can return the sum of this list
    4. Functions for interating in an IO context:
        | Function	| Behaviour   	|
        |---	        |---	|
        |  mapM 	    | Takes an IO action and a regular list, performing the action on each item in the list, and returning a list in the IO context	|
        |  mapM_ 	    | Same as mapM, but it throws away the values (note the underscore)  	                                                        |
        |  replicateM 	| Takes an IO action, an Int n, and then repeats the IO action n times, returning the results in an IO list                   	|
        |  replicateM_ 	| Same as replicateM, but it throws away the results                                                                          	|
    5. Quick check 22.2 Write your own version of replicateM, myReplicateM, that uses mapM
        ```
        myReplicateM :: Monad m => Int -> m a -> m [a]
        myReplicateM n function = mapM (\ _ ->  function ) [1 .. n]
        ```
    6. The primary purpose of having an IO type is to separate functions that absolutely must work in I/O with more general ones.
    7. **getContents** action lets you treat the I/O stream for STDIN as a list of characters. `getContents` action reads input until it gets an end-of-file signal. [getContents](https://stackoverflow.com/questions/21189325/haskell-how-getcontents-works/21190648#:~:text=getcontents%20reads%20all%20the%20stdin%2C%20but%20it%20works%20lazily.%20it%20returns%20a%20thunk%2C%20)  reads all the stdin, but it works lazily. It returns a [thunk](https://wiki.haskell.org/Thunk), it is a promise to return some value when you ask it (force the thunk).
       1. For text file , its the end of file
       2. for user input, it is Ctrl-D
    8. With getContents, you can rewrite your program, completely ignoring IO until later.  Becos if you treat the user input as a regular lazy list of Chars, you can abstract out nearly all of your non-I/O code much more easily. ie.only need to treat list as I/O when first received.
    9. -- Quick check 22.3    Use lazy I/O to write a program that reverses your input and prints it back to you.
        ```
        mainLazyReverse :: IO ()
        mainLazyReverse = do
            userInput <- getContents
            let reversee = reverse userInput
            putStr reversee
        ```
    10. To run the program in prompt: Read also http://learnyouahaskell.com/input-and-output
        1.  ghc --make summary.hs
        2.  ./summary
        3.  <ctdl-d> to end and get results of computation
    11.  Data.List.Split module contains a more generic function than lines, splitOn, which splits a String based on another String. Data.List.Split isn’t part of base Haskell, but is included in the Haskell Platform. If you aren’t using the Haskell Platform, you may need to install it. The splitOn function is a useful one to know when processing text. Here’s how lines could be written with splitOn.
    12. `splitOn` is same as `lines` but need the Data.List.Split module
        1.   map the read function over your new lists to get list of Ints.
            ```
            toInts :: String -> [Int]
            toInts = map read . lines
            ```
        2. apply toInts to userInput from getContents
    13. A nice summing program with IO
        ```
        import System.Environment
        import Control.Monad


        toInts :: String -> [Int]
        toInts = map read . lines

        main :: IO ()
        main = do
            putStrLn "Input integers to sum"
            userInput <- getContents        --- treat the I/O stream for STDIN as a list of lazy characters.
            let numbers = toInts userInput  -- make the list of char ie String into a list of Int
            print (sum numbers)

        ```
    14. A square then sum program
       ```
       import System.Environment
       import Control.Monad


        toInts :: String -> [Int]
        toInts = map read . lines

        main :: IO ()
        main = do
            putStrLn "Input integers to square and then sum"
            userInput <- getContents       --- treat the I/O stream for STDIN as a list of lazy characters.
            let numbers = toInts userInput -- make the list of char ie String into a list of Int
            let squares = map (^2) numbers
            print (sum squares)
       ```
19. Ch23.0: Working with text and Unicode
    1.  `Text` doesnt use lazy evaluation. To use lazy text, use Data.Text.Lazy which has same interface as Data.Text. `Data.Text` is preferred over `String`. For qualified import
        ```
        import qualified Data.Text as T

        T.pack :: String -> T.Text
        T.unpack :: T.Text -> String
        ```
    2. Data.Text has two functions. but Avoid converting back and forth between Text and String due to cost
       1. `pack`
       2. `unpack`
       3. example:
       ```
       firstWord :: String
       firstWord = "pessimism"
       secondWord :: T.Text
       secondWord = T.pack firstWord
       thirdWord :: String
       thirdWord = T.unpack secondWord
       ```
    3. literal strings problem to define Text.
        1. literal string cant define Text
            ```
            myWord :: T.Text
            myWord = "dog"  -- a String

            -- Error : Couldn't match expected type 'T.Text' with actual type '[Char]'
            ```
       2. Literal numbers like Int, Integer, Double dont have literal strings problem and can define text.
            ```
            myNum1 :: Int
            myNum1 = 3
            myNum2 :: Integer
            myNum2 = 3
            myNum3 :: Double
            myNum3 = 3
            ```
       3. [2withExtension.hs]To overcome literal string cant define Text, use `OverloadedStrings`.2 solutions
          1. as a flag: `$ ghc text.hs -XOverloadedStrings`
          2. PREFERRED: a `LANGUAGE` pragma at top of file. {-# LANGUAGE <Extension Name> #-}
            ```
            {-# LANGUAGE OverloadedStrings #-}
            import qualified Data.Text as T

            aWord :: T.Text
            aWord = "Cheese"

            main :: IO ()
            main = do  print aWord
            ```
    4. Useful language extensions:
         | Extensions  	        | objective                                                                                                 	|
         |---	                |---	                                                                                                        |
         | OverloadedStrings  	| make string literals as text  	                                                                            |
         | ViewPatterns  	    | support more-sophisticated pattern matching                                                               	|
         | TemplateHaskell  	    | Provides tools for Haskell metaprogramming                                                               	|
         | DuplicateRecordFields | solve lesson 16, where using the same field name for different types using record syntax causes a conflict  	|
         | NoImplicitPrelude     | allows you to not use the default Prelude so you can customized your own Prelude                             |
    5. After qualified import, use functions on Text like string with preface `T`. Almost any useful function for working with strings works on text and has its own Textversion. Useful common functions:
        1. T.lines : split into list of strings
        2. T.unlines: join a [String(s)] into a string with \n no whitespace
        3. T.words : same as lines, but it works for any whitespace characters, rather than just new lines
        4. T.unwords : join a [String(s)] into a String
        5. T.splitOn : splitOn is part of the Data.List.Split module and included in Data.Text so no additional import is needed. splitOn lets you split up text by any substring of text and separate the 2 parts by `","`
        6. T.intercalate: join a list of 2 strings into a string by replacing `","` with the joining text(s)
             ```
             {-# LANGUAGE OverloadedStrings #-}
             import qualified Data.Text as T

             -- 1. lines and unlines
             sampleInput :: T.Text
             sampleInput = "this\nis\ninput"
             GHCi>T.lines sampleInput
             ["this","is","input"]
             GHCi> T.unlines (T.lines sampleInput)
             "this\nis\ninput\n"


             -- 2. words and unwords
             someText :: T.Text
             someText = "Some\ntext for\t you"
             GHCi> T.words someText
             ["Some","text","for","you"]
             GHCi> T.unwords (T.words someText)
             "Some text for you"

             -- 3. splitOn and intercalate(join)
             breakText :: T.Text
             breakText = "simple"
             break2Text :: T.Text
             break2Text = "simple to"
             exampleText :: T.Text
             exampleText = "This is simple to do"
             GHCi> T.splitOn breakText exampleText
             ["This is "," to do"]
             GHCi> T.intercalate breakText (T.splitOn breakText exampleText)
             "This is simple to do"
             >>> T.splitOn break2Text exampleText
             ["This is "," do"]
             -- >>> T.intercalate break2Text (T.splitOn break2Text exampleText)
             -- "This is simple to do"

             ```
        7. **Exception**: there is no "++" for Text. Solve with **`Monoid` and `Semigroup` which can combine like types and concatenate lists of the same type**. use import Semigroup and `<>` or `mconcat`. Because String is also an instance of Monoid and Semigroup, strings can be combined in the same way.
             ```
             {-# LANGUAGE OverloadedStrings #-}
             import qualified Data.Text as T

             import Data.Semigroup --to use <> and mconcat to join Text since there is no T.++

             combinedTextMonoid :: T.Text
             combinedTextMonoid = mconcat ["some"," ","text"]
             combinedTextSemigroup :: T.Text
             combinedTextSemigroup = "some" <> " " <> "text"

             >>> combinedTextMonoid
             -- "some text"
             -- >>> combinedTextSemigroup
             -- "some text"
             ```
    6. [3highlightText.hs]A highlight program to search a Sanskrit word in text and highlight it. ![alt text](unit4/lesson23/highlightText.png?raw=true "Highlight program")
        ```
        highlight :: T.Text -> T.Text -> T.Text
        highlight query fullText = T.intercalate highlighted pieces
            where pieces = T.splitOn query fullText
                  highlighted = mconcat ["{",query,"}"]
        ```
    7. TIO.putStrLn, you can print your Text type just as you would String so can apply Monoid mconcat or Semigroup <>. Any IO action you’ve used related to the String type has an equivalent in Data.Text.IO. e.g
        ```
        import qualified Data.Text.IO as TIO

        Examples
        1. TIO.putStrLn
        2. name <- TIO.getLine
        3. userInput <- TIO.getContents


        {-# LANGUAGE OverloadedStrings #-}
        import qualified Data.Text as T
        import qualified Data.Text.IO as TIO
        import Data.Semigroup --to use <> and mconcat to join Text since there is no T.++

        helloPerson :: T.Text -> T.Text
        helloPerson name = "Hello" <> " " <> name <> "!"

        main :: IO ()
        main = do
            TIO.putStrLn "Hello! What's your name?"
            name <- TIO.getLine
            let statement = helloPerson (name)
            TIO.putStrLn statement
        ```
    8. There is no T.read so use ` map (read . T.unpack) . T.lines`
        ```
        toInts :: T.Text -> [Int]
        toInts = map (read . T.unpack) . T.lines
        ```
    9. Sometimes we use lazy.IO. See unit4/lesson23/l23_2exercises.hs
        ```
        {-# LANGUAGE OverloadedStrings #-}
        import qualified Data.Text.Lazy as T
        import qualified Data.Text.Lazy.IO as TIO
        ```
20. Ch24.0 Working with file with IO action
    1.  `import System.IO`
    2.  openFile :: FilePath -> IOMode -> IO Handle
    3.  type FilePath = String
    4.  For purpose of action with file: `data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode`
    5.  `IO Handle` : The Handle type is a file handle that lets you pass around a reference to a file.IO type means that you have a handle in the context of IO. In order to get this file handle, you’ll ultimately be doing the work in your main IO action.
    6.  `hClose` (for handle close): whenever you open a file, you use hClose to close it when you are finished.
    7.  `hPutStrLn` & `hGetLine`. The only difference between these two functions and putStrLn and getLine is that you need to pass in a handle.
        1.  `putStrLn` is a specific instance of `hPutStrLn`. In `hPutStrLn` the handle is assumed to be stdout.
        2.  `getLine` is `hGetLinewhere` the handle is stidn.
    8. Simple IO tools for file
        ```
        readFile :: FilePath -> IO String
        writeFile :: FilePath -> String -> IO ()
        appendFile :: FilePath -> String -> IO ()
        ```
    9. Count file contents metadata. Progam that takes a file as an argument, and then count the characters, words, and lines in the file. The program will display this data to the user and append the info to a stats.dat file.
        ```
        --1. getCounts collects character, word, and line count info into a tuple
        getCounts :: String -> (Int,Int,Int)
        getCounts input = (charCount, wordCount, lineCount)
            where charCount = length input
                wordCount = (length . words) input
                lineCount = (length . lines) input
        --2. Report summary of file contents with unwords and show
        countsText :: (Int,Int,Int) -> String
        countsText (cc,wc,lc) =  unwords ["chars: "
                                        , show cc
                                        , " words: "
                                        , show wc
                                        , " lines: "
                                        ,  show lc]

        -- >>> (countsText . getCounts) "this is\n some text"
        -- "chars:  18  words:  4  lines:  2"

        -- 3. A program that put all the metadata into a file
        main :: IO ()
        main = do
            args <- getArgs           -- need import System.Environment
            let fileName = head args
            input <- readFile fileName
            let summary = (countsText . getCounts) input
            appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])
            putStrLn summary

        -- Steps to run
        -- 1. ghc --make 3fileCounts.hs
        -- 2. ./3fileCounts hello.txt
        -- 3. cat stats.dat
        ```
    10. `getArgs` The type of getArgs is IO [String]. When you bind it with <-, as in the OP, the bound symbol (args) gets the type [String], i.e. a list of strings. The [getArgs](https://stackoverflow.com/questions/49055999/how-does-getargs-work#:~:text=the%20type%20of%20getargs%20is%20io%20%5Bstring%5D) function isn't interactive. It'll just return the arguments supplied from the command line, if any were supplied.
    11. Why `/fileCounts stats.dat` fail? Bcos readFile doesn’t close the file handle and just returns the results of hGetContent. lazy evaluation, if readFile closes the handle, won't be able to use the contents of the file. This is because a function acting on the contents of the file wouldn’t be called until after the file handle was closed. Under the hood:
        ```
        readFile :: FilePath -> IO String
        readFile name = do
        inputFile <- openFile name ReadMode
        hGetContents inputFile
        ```
    12. Lazy eval problems: although lazy I/O can be powerful, it can also lead to nasty bugs. ![alt text](unit4/lesson24/failLazyEval.png?raw=true "Problem with closing a file before we use it when using lazy evaluation")
        1.  Your input isn’t used until you define summary but..
        2.  Summary isn’t used until you call appendFile
        3.  Because appendFile performs an IO action, it does force summary to be evaluated, which forces input to be evaluated.
        4.  Real problem is that hClose closes the file immediately because it’s an IO action and must happen as soon as you evaluate it.
            ```
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
            ```
        5.  But by put hClose after appendFile because that’s when summary is finally evaluated
            ```
            appendFile (mconcat [fileName, " ",summary, "\n"])
            hClose file
            ```
        6.  Doing 5 will go back where you started; you’re closing the file after you need a new han-dle!
        7. (Solution for Lazy) needs to evaluate summary before you write to the file by move putStrLn summary before you write to the file. so it will
           1. force summary to be evaluated first.
           2. Then close the handle,
           3. finally appending the file.
           4. ![alt text](unit4/lesson24/fixLazyEval.png?raw=true "Fix for lazy evaluation bugs above")
            ```
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
            ```
    13. Count file contents metadata. **Best Solution** use a strict (nonlazy) type as Data.Text is preferred over String when working with text data. Data.Text is a strict data type (it doesn’t use lazy evaluation). Rewrite the original program by using the Text type, and problem will be solved! Strict evaluation means that your I/O code works just as you’d expect it to.
        ```
        {-# LANGUAGE OverloadedStrings #-}
        import System.IO
        import System.Environment
        import qualified Data.Text as T
        import qualified Data.Text.IO as TI

        getCounts :: T.Text -> (Int,Int,Int)
        getCounts input = (charCount, wordCount, lineCount)
                where charCount = T.length input
                    wordCount = (length . T.words) input
                    lineCount = (length . T.lines) input


        countsText :: (Int,Int,Int) -> T.Text
        countsText (cc,wc,lc) = T.pack (unwords ["chars: ", show cc,
                                                " words: ", show wc,
                                                " lines: ",  show lc])


        main :: IO ()
        main = do
            args <- getArgs
            let fileName = head args
            input <- TI.readFile fileName
            let summary = (countsText . getCounts) input
            TI.appendFile "stats.dat"
                    (mconcat [(T.pack fileName), " ",summary, "\n"])
            TI.putStrLn summary

        -- Steps to run
        -- 1. ghc --make filename.hs
        -- 2. ./filename hello.txt
        -- 3. cat stats.dat
        ```
    14. **IMPORTANT** Lazy vs Strict Evaluation
        1.  Lazy Evaluation for: program that reads a single file + little I/O work
        2.  Strict Evaluation for: > moderate complex e.g A)  reading + writing files; B) operations with important order
    15. copy and renama a file program
        ```
        {-# LANGUAGE OverloadedStrings #-}
        import System.IO
        import System.Environment
        import qualified Data.Text as T
        import qualified Data.Text.IO as TI

        main :: IO ()
        main = do
            args <- getArgs
            let source =  args !! 0
            let dest = args !! 1
            input <- TI.readFile source
            TI.writeFile dest input

        -- Steps
        -- 1. ghc --make l24_1exercises.hs
        -- 2. ./l24_1exercises hello.txt hello2.txt
        -- 3. (check that hello2.txt is generated)
        ```
20. Ch25.0: Working with Binary data
    1.  ByteString
        1.  allows you to treat raw binary data as though it were a regular string.
        2.  It is another important type that’s similar to String and Text.
        3.  It is an array of bytes and not a type of text
        4.  is an efficient way to deal with any streams of binary data.
        5.  **import** `import qualified Data.ByteString as B`
        6.  There are 256, or 28 (8 bits) ASCII characters, so every possible byte can be represented as an ASCII character.
        7.  Use the `Over-loadedStrings` extension for literal ASCII strings to represent vectors of byte.
    2. `ByteString.Char8` allows use of the same core functions for working with text as `Data.Text` does.
        1.  ByteString.Char8’s unpack works just like Data.Text’s unpack.
        2.  Data.ByteString **doesn’t allow** you to treat bytes just like Char, so instead you use Data.ByteString.Char8. Changing ByteString to List of Byte.
            ```
            import qualified Data.ByteString.Char8 as BC  --NOT Data.ByteString

            sampleBytes :: B.ByteString
            sampleBytes = "Hello!"

            B.unpack :: BC.ByteString -> [GHC.Word.Word8]
            BC.unpack :: BC.ByteString -> [Char]

            bcInt :: BC.ByteString
            bcInt = "6"

            bcToInt :: BC.ByteString -> Int
            bcToInt = read .  BC.unpack
            ```
        3. B.unpack now uses the ByteString representation from ByteString.Char8.
        4. Can now treat ByteStrings as plain ASCII text
    3. GlitchArt program
       1. Take a filename argument from the user.
       2. Read in the binary data for the image file -- by using BC.readFile.
       3. Randomly alter bytes in the image data -- BEST use code instead of IO.
       4. Write a new file containing the glitched image
       5. ![alt text](unit4/lesson25/basicglitcher.png?raw=true "Basic layout of glitcher.hs")
          1. If the glitched variable doesn’t need to be an IO type, line can be changed so that glitched is a regular variable : `let glitched = imageFile`
    4. GlitchArt Program random byte altering process: a function that will convert an Int to a Char.
       1. Use Int -> Char with toEnum since Char is a member of Enum.
       2. Constraint Char to 0 - 255 with mod 255
       3. Code to change int to Char
           ```
           intToChar :: Int -> Char
           intToChar int =  toEnum safeInt
               where safeInt = int `mod` 255
           ```
       2. Convert Char to ByteString with BC.unpack (see unit4/lesson25/1byteString.hs). Goal: make an Int into a single byte represented as a BC.ByteString
       5. Because BC.pack requires a string, you need to put your Char inside a list.
         ```
         intToBC :: Int -> BC.ByteString
         intToBC int = BC.pack [intToChar int]
         ```
       6.  write the code to replace a byte with this value with replaceByte function which will take : ![alt text](unit4/lesson25/replaceByte.png?raw=true "replaceByte removes a byte and replaces it with a new one")
           1.  the location of the byte to be replaced
           2.  the Int value of the new Char/Byte to go there,
           3.  and the bytes of the image file.
       7. Use BC.splitAt to split your byte around the target byte. `BC.splitAt` will give you a pair of values representing the first part of the data and then the rest (just like calling take and drop at the same time).
       8. Then you’ll drop one from the rest of the bytes to make room for your new byte.
       9. Finally, you’ll concatenate the new byte in the middle of these two sections.
          ```
          -- Step 6-9
          replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
          replaceByte loc charVal bytes = mconcat [before,newChar,after]
               where (before,rest) = BC.splitAt loc bytes
                      after = BC.drop 1 rest
                      newChar = intToBC charVal
           ```
       10. Now IO action. using randomRIO from `System.Random. randomRIO` will take a pair of values in a tuple and randomly give you a number in that range. The IO action will be named `randomReplaceByte`.
       11. `randomReplaceByte` pick two random numbers: one for the Char, and one for the location, both randomly given the 2 respective ranges.
            ```
            randomReplaceByte :: BC.ByteString -> IO BC.ByteString
            randomReplaceByte bytes = do
                    let bytesLength = BC.length bytes
                    location <- randomRIO (1,bytesLength)
                    charVal <- randomRIO (0,255)
                    return (replaceByte location charVal bytes)
            ```
       12.  error with random: solution : `cabal install --lib random`
            ```
                    2glitcher.hs:4:1: error:
                Could not find module ‘System.Random’
                Use -v (or `:set -v` in ghci) to see a list of the files searched for.
            4 | import System.Random
            ```
       13.  Minimal GlitchArt Program code for mainGlitchLittle:
            ```
            import System.Environment
            import qualified Data.ByteString as B
            import qualified Data.ByteString.Char8 as BC
            import System.Random

            intToChar :: Int -> Char
            intToChar int =  toEnum safeInt
                where safeInt = int `mod` 255

            intToBC :: Int -> BC.ByteString
            intToBC int = BC.pack [intToChar int]

            replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
            replaceByte loc charVal bytes = mconcat [before,newChar,after]
                where (before,rest) = BC.splitAt loc bytes
                    after = BC.drop 1 rest
                    newChar = intToBC charVal

            randomReplaceByte :: BC.ByteString -> IO BC.ByteString
            randomReplaceByte bytes = do
                let bytesLength = BC.length bytes
                location <- randomRIO (1,bytesLength)
                charVal <- randomRIO (0,255)
                return (replaceByte location charVal bytes)

            main :: IO ()
            main = do
                args <- getArgs
                let fileName = head args
                imageFile <- BC.readFile fileName
                glitched <- randomReplaceByte imageFile  -- GLITCHING POINT
                let glitchedFileName = mconcat ["glitched_",fileName]
                BC.writeFile glitchedFileName glitched
                print "all done"
            ```
       14.  Commands to run gltichArt program:
            ```
            $ ghc 2glitcher.hs
            $ ./2glitcher lovecraft.jpg
            ```
       15. To enhance glitchArt, replace **randomReplaceByte with randomSortSection (here)**
           1.  take a subsection to sort from a point with BC.splitAt.
           2.  Split the second part into a chunk of fixed size.
           3.  Sort the chunk
           4.  Put it all back together with `mconcat`.
           5.  This sortSection function, which takes a starting point of the section, a size of the section, and the byte stream
                ```
                sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
                sortSection start size bytes = mconcat [before,changed,after]
                    where (before,rest) = BC.splitAt start bytes
                          (target,after) = BC.splitAt size rest
                          changed =  BC.reverse (BC.sort target)
                ```
           6. create an IO action that picks a random starting point with sortSection
               ```
               randomSortSection :: BC.ByteString -> IO BC.ByteString
               randomSortSection bytes = do
                    let sectionSize = 25
                    let bytesLength = BC.length bytes
                    start <- randomRIO (0,bytesLength - sectionSize)
                    return (sortSection start sectionSize bytes)
               ```
           7. Arbitrarily pick size and location: ![alt text](unit4/lesson25/sortSectionIO.png?raw=true "Random location and size with IO action")
           8. Revised program code for mainGlitchBetter
            ```
            import System.Environment
            import qualified Data.ByteString as B
            import qualified Data.ByteString.Char8 as BC
            import System.Random

            sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
            sortSection start size bytes = mconcat [before,changed,after]
                where (before,rest) = BC.splitAt start bytes
                      (target,after) = BC.splitAt size rest
                      changed =  BC.reverse (BC.sort target)

            randomSortSection :: BC.ByteString -> IO BC.ByteString
            randomSortSection bytes = do
                 let sectionSize = 25
                 let bytesLength = BC.length bytes
                 start <- randomRIO (0,bytesLength - sectionSize)
                 return (sortSection start sectionSize bytes)

            main :: IO ()
            main = do
                args <- getArgs
                let fileName = head args
                imageFile <- BC.readFile fileName
                glitched <- randomSortSection imageFile  -- GLITCHING POINT
                let glitchedFileName = mconcat ["glitched_",fileName]
                BC.writeFile glitchedFileName glitched
                print "all done"
            ```
       16. Combo GlitchArt by use `randomSortSection` (twice) and `randomReplaceByte` (three times) for mainGlitchCombo.
            ```
            main :: IO ()
            main = do
                    args <- getArgs
                    let fileName = head args
                    imageFile <- BC.readFile fileName
                    glitched1 <- randomReplaceByte imageFile --- THE POINT OF GLITCHING
                    glitched2 <- randomSortSection glitched1 --- THE POINT OF GLITCHING
                    glitched3 <- randomReplaceByte glitched2 --- THE POINT OF GLITCHING
                    glitched4 <- randomSortSection glitched3 --- THE POINT OF GLITCHING
                    glitched5 <- randomReplaceByte glitched4 --- THE POINT OF GLITCHING
                    let glitchedFileName = mconcat ["glitched_",fileName]
                    BC.writeFile glitchedFileName glitched5
                    print "all done"
            ```
       17. Use foldM from **Control.Monad** to reduce Step 16 typo and use lambda for chaining.
           1.  `mapM` generalizes map to monads; `foldM` does the same for folding.
           2.  take your original imageFile as the initial values, plus a list of IO actions to transform the file with a simple lambda.
            ```
            main :: IO ()
            main = do
                args <- getArgs
                let fileName = head args
                imageFile <- BC.readFile fileName
                glitched <- foldM (\bytes func -> func bytes) imageFile
                                                                    [randomReplaceByte,randomSortSection,randomReplaceByte,randomSortSection,randomReplaceByte]
                let glitchedFileName = mconcat ["glitched_",fileName]
                BC.writeFile glitchedFileName glitched
                print "all done"
            ```
       18. GlitchArt Code with a list of glitch Actions
            ```
            import System.Environment
            import qualified Data.ByteString as B
            import qualified Data.ByteString.Char8 as BC
            import System.Random ( randomRIO )
            import Control.Monad ( foldM )

            glitchActions :: [BC.ByteString -> IO BC.ByteString]
            glitchActions = [randomReplaceByte
                            ,randomSortSection
                            ,randomReplaceByte
                            ,randomSortSection
                            ,randomReplaceByte]

            -- Using stringed glitchActions
            main :: IO ()
            main = do
                        args <- getArgs
                        let fileName = head args
                        imageFile <- BC.readFile fileName
                        glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
                        let glitchedFileName = mconcat ["glitched_",fileName]
                        BC.writeFile glitchedFileName glitched
                        print "all done"
            ```