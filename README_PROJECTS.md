28. Ch34.0 ORGANIZING HASKELL CODE WITH MODULES
    1. You  have  a  type  for  books  and  a  type  for  magazines.  Each  has  the same field names, but they represent different things. Both types are written using record syntax, which creates a problem. Record syntax automatically creates accessor functions title and price. Unfortunately, this causes an error because you’re attempting to define two functions of the same name.
        ```
        data Book = Book
                { title :: String
                , price :: Double }

        data Magazine = Magazine
                { title :: String
                , price :: Double }
        ```
    2.  Head list
        1.  Problem: Languages such as Lisp and Scheme will return the empty list as the result of calling head on an empty list, but Haskell’s type system doesn’t allow this (because the empty list is usually a different type than values in the list itself). In **Prelude**
            ```
                head                    :: [a] -> a
                head (x:_)              =  x
                head []                 =  errorEmptyList "head"
            ```
        2. Solution: But you can come up with a solution to this problem if you constrain head to work on members of the Monoid type class. All Monoids are required to define a mempty element. The mempty element represents the empty value for instances of Monoid. **List is an instance of Monoid, and mempty is just the empty list, []**. For members of Monoid, you can return the mempty element when you have an empty list. Here’s your new, safer version of head.
            ```
            -- recall
            class Monoid m where
                mempty :: m
                mappend :: m -> m -> m
                mconcat :: [m] -> m

            head :: Monoid a => [a] -> a
            head (x:xs) = x
            head [] = mempty
            ```
        3. Test: To test your new head function, you need an example of a list with values that are members of Monoid. In this case, you’ll use an empty list of lists (remem-ber, the elements of your list must be an instance of Monoid).
            ```
            An example list that’s a list of values that are an instance of Monoid
            example :: [[Int]]
            example = []
            ```
        4. Confusion in ghci. ![Alt text](unit6/lesson34/preludevsMainHead.png?raw=true "Prelude vs Main head function") <p align="center"> Prelude vs Main head function </p>
        5.  When you don’t explicitly tell Haskell that you’re in a module, Haskell assumes that you’re the Main module. Make explicit at the top of your file with **module Main where**
            ```
            module Main where
            head :: Monoid a => [a] -> a
            head (x:xs) = x
            head [] = mempty

            example :: [[Int]]
            example = []
            ```
        6. In GHCI, *Qualify* your function’s name with the name of the module. You use **Main.head** to specify your head, and **Prelude.head** to use the default Prelude definition of head.
            ```
            GHCi> Main.head example
            []
            GHCi> Prelude.head example
            ***Exception: Prelude.head: empty list
            ```
    3. A palindrome program in 2 files. Reasons ![Alt text](unit6/lesson34/simplePalindrome.png?raw=true "Draft Palindrome") <p align="center"> Draft Palindrome </p>
       1. correctly identifies racecar as a palindrome, but fails to identify A man, a plan, a canal: Panama!
            ```
            *Main> main
            "Enter a word and I'll let you know if it's a palindrome!"
            racecar
            "it is!"
            *Main> main
            "Enter a word and I'll let you know if it's a palindrome!"
            A man, a plan, a canal: Panama!
            "it's not!
            ```
          1.  Need preprocessing for your strings to strip out whitespace, remove punctuation, and ignore case and  pull out your palindrome code into a separate file for two reasons bcos
              1.  First, it makes your Main cleaner
              2.  Second, you can then more easily reuse your palindrome code in other programs.
        2. The module’s name will be Palindrome, so your code should be in a file named **Palindrome.hs**. Your Palindrome module will have a **function, also named isPalindrome**, which will be the function used by the Main module. See unit6/lesson34/Palindrome.hs or  ![Alt text](unit6/lesson34/robustPalindrome.png?raw=true "Robust Palindrome") <p align="center"> Robust Palindrome </p>
           1. `module Palindrome where` would export all the functions defined in Palindrome.hs.
           2. `module Palindrome(isPalindrome) where` **only** export `isPalindrome` function. Hence, depending on which function you want, list all the required functions you want to export in parentheses after the module name. Alernative format is
                ```
                module Palindrome
                        ( isPalindrome
                        ) where
                ```
           3. Selectively import few functions from the `Data.Char` module instead of bulk import by qualifying. Benefit : reduces the possibility that you’ll have an unexpected namespace collision when performing a nonqualified import.  ![Alt text](unit6/lesson34/helperFunctions.png?raw=true "Palindrome Helper Functions") <p align="center"> Palindrome Helper Functions </p>
                ```
                import Data.Char (toLower,isSpace,isPunctuation)
                ```
           4. Your Palindrome module doesn’t have a main because it’s just a library of functions. Even without a main, you can still load your file into GHCi and test it out
                ```
                    *Palindrome> isPalindrome "racecar"
                    True
                    *Palindrome> isPalindrome "A man, a plan, a canal: Panama!"
                    True
                ```
    4. Different ways to write import module :
        1. where program is in the module file
           1. method 1: `module Palindrome (isPalindrome, preprocess) where ` .
           2. method 2: in more structured format
                ```
                module Palindrome(
                    isPalindrome
                    ,preprocess
                    ) where
                ```
            3. and run program with
                ```
                ➜  lesson34 git:(master) ✗ ghci Palindrome.hs
                *Palindrome> isPalindrome "A man, a plan, a canal, Panama!"
                ```
        2. with a separate Main.hs file
           1. method 3: Qualified `import qualified Module as X` like `import qualified Data.Text as T`
           2. method 4: with separate Main.hs and the Palindrome module (from Palindrome.hs) must be in the same directory so Palindrome.isPalindrome can be derived
                ```
                module Main where
                import qualified Palindrome  -- as Palin  (Alterantive short name)

                main :: IO ()
                main = do
                    print "Enter a word and I'll let you know if it's a palindrome!"
                    text <- getLine
                    let response = if Palindrome.isPalindrome text -- = if Palin.isPalindrome text
                                    then "it is!"
                                    else "it's not!"
                    print response
                ```
            3. Run program with
                ```
                ghc Main.hs
                ./Main
                ```
    5. Note `import qualified Palindrome` vs `import Palindrome ()` in unit6/lesson34/Main.hs. [Difference](https://stackoverflow.com/questions/11274864/what-does-qualified-mean-in-import-qualified-data-list-statement#:~:text=If%20you%20do%20a%20qualified%20import%2C%20you%20have%20to%20prefix%20the%20name%20with%20the%20module%20it%27s%20imported%20from.)
    6. With Main.hs, we learn how to organize programs into separate files and compile them into a single program. You also learned how to export specific functions from your modules while hiding the rest.
    7. Refactor String as Text for program in chapter exercises in unit6/lesson34/l34_exercises
29. Ch35.0 Building Projects with Stack
    1.  Building projects need a proper build automation. Haskell too has a powerful build tool: stack. Stack automates and manages several parts of Haskell projects:
        1.  Provides an isolated installation of GHC per project to ensure that you’re always using the correct version
        2.  Handles the installation of packages and their dependencies
        3.  Automates the building of the project
        4.  Aids you in organizing and running tests on your project
    2. For every new project, do
       1. make sure that stack is up-to-date with `stack update`
       2. stack new example with `stack new palindrome-checker`.  This command causes stack to create a new project from a default template via a new directory named palindrome-checker with the following files and directories
          1. Overview:
             1. LICENSE
             2. src
             3. Setup.hs
             4. stack.yaml
             5. app
             6. test
             7. palindrone-checker.cabal
          2. Other templates are available in https://github.com/commercialhaskell/stack-templates is a source for many
          3. **cabal** palindrome-checker.**cabal**, your project configuration file contains all the metadata related to your project e.g project name, version, description, *libaries* files location, haskell language
              1.  stack separates the code for your libraries vs the code for running your program into separate directories.
              2.  2 impt lines - `hs-source-dirs` (in **both** library and executable sections) and `exposed-modules`.
              3.  `hs-source-dirs` value tells you which subdirectory of your project your library files live in. Default is `src`
              4.  `exposed-modules` value tells you which libraries you’re using. `Lib` module default location is in `src/Lib.hs`. To add more values to `exposed-modules`, do
                   ```
                   exposed-modules:    Lib,
                                       Palindrome,
                                       Utils
                   ```
          4. Small project , all the library functions can go into `src/Lib.hs`.
          5. Like the library section, the executable section also tells which file your `main` is located with the main-is value.
          6. Code for running program (executable): `app` directory (specified by `hs-source-dirs`) [where the program’s `Main` module lives via a Main.hs file inside]
          7. Gist: stack also automatically creates three directories for you:
              1.  app
              2.  src
              3.  test (more in unit6/lesson36)
          8. Difference

            |  Library module               |Executable module                   |
            | -------------                 | -------------                      |
            | src                           | app                                |
            | src/Lib.hs.                   | app/Main.hs                        |
            | Lib.hs has the functions      | min& import Lib module from Lib.hs |

          9. Main module (generated by stack)
                ```
                module Main where

                import Lib      --> from src/Lib.hs

                main :: IO ()
                main = someFunc  -->> comes from Lib module in src/Lib.hs
                ```
          10. Lib module (generated by stack)

                ```
                module Lib
                    ( someFunc
                    ) where

                someFunc :: IO ()
                someFunc = putStrLn "someFunc"
                ```
          11. Your `Main` module logic should be minimal, relying primarily on library functions defined in the `Lib` module.

       3.  Overwrite `Lib.hs` stack with functions bcos simple project. Default original:
           ```
           module Lib
               ( someFunc
               ) where

           someFunc :: IO ()
           someFunc = putStrLn "someFunc"
           ```
       4.  Overwrite `Main.hs` which is essential to building your executable, it goes in the app directory (this is declared in your project’s .cabal file!). Rem `import Lib` **This time you won’t use a qualified import.** Default original:
           ```
           module Main where
           import Lib -- ** impt
           main :: IO ()
           main = someFunc
           ```
       5.  A. Add edits to your **.cabal** file (unit6/lesson36/l36exercise/palindrome-full-text/palindrome-full-text.cabal) to tell stack about any modules you’re depending on e.g. `Data.Text` into the `build-depends:` to library and executable and also [test-suite](https://github.com/1Regina/GetProgrammingWithHaskell/blob/master/README_PROJECTS.md#:~:text=properties%20are%20upheld.-,3%20things%20to%20do%3A,-QuickCheck%20steps) ![Alt text](unit6/lesson35/cabalEdits.png?raw=true "Add packages to dependencies") <p align="center"> Additions to build dependencies </p>
            ```
            test-suite palindrome-testing-test
                ......
                build-depends:
                    base >=4.7 && <5
                    , palindrome-testing
                    , QuickCheck
                    , text
                    , quickcheck-instances
                default-language: Haskell2010
            ```
       6.  A. With everything together to build project, ensures that stack is using the correct version of GHC, run setup **in project directory** with:
           ```
           stack setup
           ```
       7.  A. Ensuring that your project is built with the version of GHC you used to write it is important. Specifying the version of GHC you want to use is done indirectly by choosing your stack resolver version. The stack resolver is set in the **stack.yaml** file: `resolver: lts-7.9`. The lts-7.9 version of the stack resolver uses GHC version 8.0.1. By default, stack uses the most recent stable resolver. Listing of the current resolver versions at www.stackage.org. For info on specific resolver, e.g 7.9 by www.stackage.org/lts-7.9 for the lts-7.9 resolver
       8. A. Build your project with `stack build`.
       9. A. Run the project  with `exec` as in `stack exec palindrone-checker-exe` using the default 's `<project-name>-exe` The name of the project is taken from .cabal file line #38 `executable palindrone-checker-exe`
       10. A. **QuickFix** For large program with `Data.Text`, add OverloadedStrings pragma to every file. Shortcut:
           1.  Solution 1: Go to .cabal file and universally apply the `OverloadString` language extension at after default-language:
           2.  after default-language:  Haskell2010 to **both** your library and executable sections of .cabal:
               ```
               extensions: OverloadedStrings
               ```
       11. B **OTHERWISE** if the [.cabal#line3](https://github.com/1regina/GetProgrammingWithHaskell/blob/master/unit6/lesson35/palindrome-checker/palindrome-checker.cabal#L3) is `-- This file has been generated from package.yaml by hpack version 0.34.4.`, then
           1.  Solution 1: **option 1** go straight to **package.yaml**#dependencies and add instead . e.g to add text. (demo-ed: unit6/lesson35/palindrome-checker1)
               ```
               dependencies:
               - base >= 4.7 && < 5
               - text
               ```
           2. Solution 2: **option 2** delete `package.yaml` file and edit the `.cabal` file `build-dependencies` for required sections (demo-ed: unit6/lesson35/palindrome-checker1WOPackageYaml)
    3.  then do `stack run`
    4.  Difference between palindrome-checker vs palindrome-checker1 is src directory. Both have package.yaml files

        | palindrome-checker                                                               | palindrome-checker1                                                                     |
        | -------------                                                                    | -------------                                                                           |
        | src directory only has Lib.hs                                                    | src directory has Lib.hs and Palindrome.hs                                              |
        | src/Lib.hs has all the preprocessing (stripWhiteSpace, stripPunctuation etc) fns | src/Palindrome.hs has all the preprocessing (stripWhiteSpace, stripPunctuation etc) fns |
        | app/Main.hs import Lib module of src/Lib.hs                                      | app/Main.hs import Palindrome module of src/Palindrome.hs                               |
    5.  Exercises.
        1.  Q35.1: (unit6/lesson35/palindrome-checker1) Cant do `extensions: OverloadedStrings` in .cabal nor package.yaml alternative (see pt 10 above under QuickFix) so retain OverloadedStrings pragma in Main.hs and Palindrome.hs. Completed below and `stack run`. *Update 15 Aug 2021: see project in unit7/lesson39/http-lesson [adding extension in package.yaml](https://github.com/snoyberg/haskellers/blob/master/package.yaml)* and suggest run http-lesson project.
            ```
            Added Palindrome.hs to src with
            {-# LANGUAGE OverloadedStrings #-}
            module Palindrome where......
            =================
            Edit: import Palindrom instead of Lib.hs in app/Main.hs with
            {-# LANGUAGE OverloadedStrings #-}
            module Main where

            import Palindrome (isPalindrome)
            import Data.Text as T

            ====================

            Add: `- text` in unit6/lesson35/palindrome-checker1/package.yaml in #line22

            dependencies:
            - base >= 4.7 && < 5
            - text
            ```
        2.  Q35.1 Attempt 2: **without package.yaml, just good old .cabal as in book** (unit6/lesson35/palindrome-checker1WOPackageYaml) Add `extensions: OverloadedStrings` in .cabal after deleting package.yaml (alternative to retain OverloadedStrings pragma in Main.hs (via .cabal file executable section) and Palindrome.hs (or Lib.hs depending what is imported in Main.hs) via .cabal file library section). Completed below and `stack run`
            ```
            Remove `OverloadString` language extension in Palindrome.hs in src with
            -- {-# LANGUAGE OverloadedStrings #-}
            module Palindrome where......
            =================
            Edit: import Palindrom instead of Lib.hs in app/Main.hs with since Palindrome is made a exposed module per cabal file and being import explicitly in Main.hs
            -- {-# LANGUAGE OverloadedStrings #-}
            module Main where

            import Palindrome (isPalindrome)
            import Data.Text as T

            ====================
            Add: `- text` in library and executable sections of .cabal file
            dependencies:
            - base >= 4.7 && < 5
            - text
            ====================
            after default-language:  Haskell2010 to **both** your library and executable sections of .cabal. add
            `extensions: OverloadedStrings`
            ```
        3. Q35.2 pizza compare from  unit 4, lesson 21
           1. Added unit6/lesson35/pizza-compare/src/Compare.hs
           2. Edited unit6/lesson35/pizza-compare/app/Main.hs. See new contents
           3. `stack run`

30. Ch36.0 Property Testing with Quickcheck (unit6/lesson36/palindrome-testingQC has package.yaml. unit6/lesson36/palindrome-testingWOPackageYaml_String package.yaml is deleted.) (unit6/lesson36/palindrome-testingWOPackageYaml_String vs unit6/lesson36/palindrome-testingWOPackageYaml_Text: support String vs Text). Devt process 1. unit6/lesson36/palindrome-testingQC 2. unit6/lesson36/palindrome-testingWOPackageYaml_String, 3. unit6/lesson36/palindrome-testingWOPackageYaml_Text

    | stack                         | palindrome-testingQC   (has package.yaml)   | palindrome-testingWOPackageYaml_String              | palindrome-testingWOPackageYaml_Text (has Data.Text for Text) |
    | -------------                 | -------------                               | -------------                                       |  -------------                                                |
    | stack ghci                    | isPalindrome "ta at" ok                     | isPalindrome "ta at" ok                             | (i) import Data.Text as T (ii) isPalindrome (T.pack "ta at")  |
    | stack run                     | returns Hello World                         | returns Hello World                                 | returns Hello World                                           |
    | stack test                    | only QuickCheck 100 tests                   | quickCheckWith 1000 tests + QuickCheck 100 tests    | quickCheckWith 1000 tests + QuickCheck 100 tests              |

    1.  Fresh project to build out functionality in unit6/lesson36/palindrome-testing/src/Lib.hs with `stack new palindrome-testing` *rename palindrome-testing directory to palindrome-testingQC
    2.  Overwrite Lib.hs with
        ```
        module Lib
            ( isPalindrome  -- but if only a few fns in this module, then export entire module by `module Lib where`
            ) where

        isPalindrome :: String -> Bool
        isPalindrome text = text == reverse text
        ```
    3. `stack test` command to automatically run crude unit tests you’ll write. Loading GHCI ~ manual testing of code
    4. Traditional Approach:
       1. unit testing = automating manual tests
       2. property testing = automating unit tests
    5. Test Driven Development = reverse (Traditional Approach) by writing tests first.
    6. Behavioural approach ~ Ruby's RSpec , Haskell's Hspec (a testing library)
    7. Manual Testing and calling GHCI from stack
       1. set up and build project
            ```
            $ cd palindrome-testing
            $ stack setup
            ...
            $ stack build
            ...
            ```
       2. Because stack is creating a safe, reproducible, isolated environment for your project, you don’t want to run ghci from the command line to interact with your project. This is because each project has its own libraries and even possibly its own version of GHC installed just for it. To safely interact with your project, you need to run `stack ghci`. For this section, rather than use the GHCi> prompt as you have throughout the book, you’ll use the actual prompt you get from stack ghci:
            ```
            $ stack ghci
            *Main Lib>
            ```
       3.  Because you’ve built your project and are running stack GHCi, the Main and Lib modules are loaded as indicated by the prompt. Next do code testing
            ```
            *Main Lib> isPalindrome "racecar"
            True
            *Main Lib> isPalindrome "racecar!" ---> this shd be palindrome
            False
            ```
       4.  To rectify "racecar!" , fix isPalindrome function in Lib.hs
            ```
            isPalindrome :: String -> Bool
            isPalindrome text = cleanText == reverse cleanText
                    where cleanText = filter (not . (== '!')) text
            ```
       5. To test edits, do `quit ghci` then restart with `stack ghci`
       6. If changes have been made only to code files and no configuration changes have been made, you can also type `:r` into GHCi to **reload your code without exiting**:
            ```
            *Main Lib Paths_palindrome_testing> :r
            Ok, three modules loaded.
            *Main Lib Paths_palindrome_testing> isPalindrome "racecar!"
            True
            ```
       7. **Assertion** unit6/lesson36/palindrome-testingQC/test/Spec.hs. Create a unit testing framework by define an assert IO action that takes a Bool (in this case, a test of a function) and prints either a passing message or a fail message.
           1. Fill out Spec.hs main
           2. import Lib module
            ```
            import Lib

            assert :: Bool -> String -> String -> IO ()
            assert test passStatement failStatement = if test
                                                    then putStrLn passStatement
                                                    else putStrLn failStatement

            main :: IO ()
            main = do
                putStrLn "Running tests..."
                assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
                assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
                assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"  -- rem the contrary case
                putStrLn "done!"
            ```
       8. exit GHCI by `:q`
       9. run the test by `stack test` in the project directory
    8.  **Property Testing** To isPalindrome other punctuation, the *isPunctuation* function in *Data.Char* is the correct solution. But there are endless punctuations to think of for testing. So a powerful solution is *property testing* and this automates much of the hassle of creating individual unit tests.
        1.  Refactor the *isPalindrome* function inside Lib module in Lib.hs into a *preprocess* function so the real testing interest is *preprocess*
        2.  To test that the output, given the input, is punctuation invariant, ie don’t care about whether the input string has punctuation.
        3.  write a function to express this property by import Data.Char (isPunctuation) (to use *isPunctuation* to cover all punctuations scenario testing) and put this function in your Spec.hs file.
        4. Import *Data.Char (isPunctuation)* and put the property function into test/Spec.hs ![Alt text](unit6/lesson36/propertyFunction.png?raw=true "Test a property in a function") <p align="center"> Test a property in a function </p>
        5. See property testing 1: functions in src/Lib.hs after `import Data.Char (isPunctuation)` to use isPunctuation to cover all punctuations scenarios
            ```
            prop_punctuationInvariant text = preprocess text ==
                                            preprocess noPuncText -- OR alternative noPuncText
                where noPuncText = filter (not. isPunctuation) text

            ```
        6. Property testing 2: function in src/Lib.hs
            ```
            prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)
            ```
        7. But step 5 and 6 do not do the property testing.
    9.  Enter **QuickCheck** for property testing. How: you supply properties that your code is supposed to uphold, and then QuickCheck automatically generates values and tests them on the functions, making sure the properties are upheld.
        1. 3 things to do: ![Alt text](unit6/lesson36/quickCheckToDo.png?raw=true "QuickCheck steps") <p align="center"> QuickCheck steps </p>
        2. First: add QuickCheck
           1. Case where package.yaml deleted: add QuickCheck to your build-depends in the .cabal file under the test-suite
                ```
                test-suite palindrome-testing-test
                .....
                build-depends:
                    base >=4.7 && <5
                    , palindrome-testing
                    , QuickCheck
                ......
                ```
           2. Case where package.yaml is used: [add dependencies. Specify version](https://github.com/1Regina/GetProgrammingWithHaskell/blob/8db5f8a8d7e08be5f959b0bcdd45c21d807b1189/unit6/lesson36/palindrome-testingQC/package.yaml#L24) but version not required for broad based dependences e.g [text](https://github.com/1Regina/GetProgrammingWithHaskell/blob/81ab0315ba7e1648e4d0fac4cb0709d5bc1b68c6/unit6/lesson35/palindrome-checker1/package.yaml#L24)
                ```
                dependencies:
                - base >= 4.7 && < 5
                - QuickCheck  >= 2.7
                ```
        3.  Second: import Test.QuickCheck at the top of Spec.hs file. `import Test.QuickCheck`
        4.  Third: call the quickCheck function on your property inside the main of Spec.hs file
            ```
            main :: IO ( )
            main = do
                quickCheck prop_punctuationInvariant
                putStrln "done!"
            ```
        5. Remember to ensure `Lib` module in Lib.hs are exporting the requisite function (e.g prop_punctuationInvariant) otherwise just avail all. See unit6/lesson36/palindrome-testingQC/src/Lib.hs top section
        6. run test by `stack test` due to different punctuation scenarios.
        7. **continue with unit6/lesson36/palindrome-testingWOPackageYaml_String (ie good old .cabal) henceforth to avoid duplicate notes documentation** Add `import Data.Char(isPunctuation)` in Lib.hs (ORIGINAL without stack install quickcheck-instances):
            ```
            import Data.Char (isPunctuation) -- to use isPunctuation to cover all punctuations scenarios test

            preprocess :: String -> String
            preprocess text = filter (not . isPunctuation) text -- filter (not . (`elem` ['!', '.', '[','\\'])) text ()  - slow one punctuation by 1 punctuation

            ```
        8. (ORIGINAL without stack install quickcheck-instances): run test with `stack test` and results after QuickCheck strategically tried 100 strings on your property, and they all passed!
            ```
            +++ OK, passed 100 tests.
            done!

            ```
    10. **Expand tests with quickCheckWith** (ORIGINAL without stack install quickcheck-instances): try 1,000 using `quickCheckWith`.
        1.  Go to unit6/lesson36/palindrome-testingWOPackageYaml_String/test/Spec.hs and edit main and run `stack test` again.
            ```
            main :: IO ( )
            main = do
                -- quickCheck prop_punctuationInvariant -- test property with only 100 tests
                quickCheckWith stdArgs { maxSuccess = 1000}  prop_punctuationInvariant -- test property with 1000 tests
                putStrLn "done!"
            ```
        2.  result from `quickCheckWith` 1000 test
            ```
            +++ OK, passed 1000 tests.
            done!
            ```
        3.  (ORIGINAL without stack install quickcheck-instances): Quick Check Exercise 36.5 on *prop_reverseInvariant* and do:
            1.  update Lib.hs to avail `prop_reverseInvariant` in module Lib
            2.  add in unit6/lesson36/palindrome-testingWOPackageYaml_String/test/Spec.hs main `quickCheckWith stdArgs { maxSuccess = 1000}  prop_reverseInvariant `
                ```
                -- (in src/Lib.hs)
                module Lib
                    ( isPalindrome
                    , preprocess
                    , prop_punctuationInvariant
                    , prop_reverseInvariant   -- add property to test
                    ) where

                import Data.Char (isPunctuation)....
                ======================
                -- (in test/Spec.hs main)
                main :: IO ( )
                main = do
                    -- quickCheck prop_punctuationInvariant -- test property with only 100 tests
                    quickCheckWith stdArgs { maxSuccess = 1000}  prop_punctuationInvariant -- test property with 1000 tests
                    quickCheckWith stdArgs { maxSuccess = 1000}  prop_reverseInvariant
                    quickCheck prop_reverseInvariant -- QuickCheck exercise 36.5
                    putStrLn "done!"
                ```
            3. results displayed
                ```
                +++ OK, passed 1000 tests.
                +++ OK, passed 1000 tests.
                +++ OK, passed 100 tests.
                done!
                ```
    11. **Expand test types**  All types that QuickCheck can automatically test must be an instance of the type class Arbitrary. but that's only a few base types. E.g `Data.Text` by default isn’t an instance of Arbitrary and won’t work with QuickCheck. (Use Version unit6/lesson36/palindrome-testingWOPackageYaml_Text henceforth)
           1.  **Remedy to expand types covered by QuickCheck:** `stack install quickcheck-instances` to install a new package to the project. Now can handle Data.Text beyond String type.
           2.  Next is refactor
               1. Refactor src/Lib.hs replacing String with T.Text and  filter, reverse with T.filter and T.reverse respectively.
               2. Refactor test/Spec.hs
                    ```
                        -- after stack install quickcheck-instances ie can handle Data.Text
                        import Test.QuickCheck.Instances  -- new!
                        import Data.Char(isPunctuation)
                        import Data.Text as T             -- new!
                    ```
               3. Add `text` to `build-depends` in the **library** section of your project’s .cabal file.
                    ```
                    library
                        ...
                        hs-source-dirs:
                            src
                        build-depends:
                            base >=4.7 && <5
                            , text  ---> ADD THIS!
                        default-languag....
                    ```
               4.  add both `text` and `quickcheck-instances` to your ****test suite build-depends** in the .cabal file
               5.  run test for **text** type `stack test`
                    ```
                        +++ OK, passed 1000 tests.
                        +++ OK, passed 1000 tests.
                        +++ OK, passed 100 tests.
                        done!
                    ```
    12. Another benefit of property testing. Just refactor and do `stack test`
    13. Summary Things done/learnt:
          1. manually testing your code by using stack ghci to ensure code build as expected.
          2. stack test command to build and run a series of simple unit tests
          3. generalized your unit testing by creating property tests with QuickCheck.
    14. Exercise is a full palindrome like in unit6/lesson34 that handles white spaces, punctuation, capital for Text instead of string. See work done in unit6/lesson36/l36exercise/palindrome-full-text
        1.  app/Main.hs
        2.  src/Lib.hs
        3.  test/Spec.hs
        4.  `build-depends` of library, executable, test-suite of unit6/lesson36/l36exercise/palindrome-full-text
        5.  stack commands used
            1.  `stack setup`
            2.  `stack build`
            3.  `stack exec palindrome-full-text-exe`
            4.  `stack ghci` then `import Data.Text as T` then `isPalindrome (T.pack "ta AT")`
            5.  `stack run`
            6.  results of `stack test` for punctuation, space, case, reverse + QC reverse (100)
                    ```
                    +++ OK, passed 1000 tests.
                    +++ OK, passed 1000 tests.
                    +++ OK, passed 1000 tests.
                    +++ OK, passed 1000 tests.
                    +++ OK, passed 100 tests.
                    done!
                    ```
31. Ch38.0 Errors in Haskell and the Either Type
    1.  Compiled fine til runtime
        1. `head` : if a Haskell program compiles, it likely runs as expected. But `head` violates this rule by making it easy to write code that compiles but then causes an error at runtime.
        2. set a compiler flag to warn of any potential problems with the code using the `-Wall` flag.
        3. in stack by adding `-Wall` to the *ghc-options value in the executable section* of the **.cabal** file. ![Alt text](unit7/lesson38/wallWarning.png?raw=true "Warnings at compile time") <p align="center"> Warnings at compile time </p>
        4. After you change your file, you need to **restart GHCi** (which will automatically rebuild your project). But building brings no complaints from the compiler. No hint of danger until runtime.
            ```
            *Main> myTake 2 [1,2,3]
            [1,2]
            *Main> myTake 4 [1,2,3]
            [1,2,3,*** Exception: Prelude.head: empty list
            ```
        5. The similar myTake below will alert an error, saying it doesn't have a pattern for the empty list.
            ```
            myTakePM :: Int -> [a] -> [a]
            myTakePM 0 _ = []
            myTakePM n (x:xs) = x : myTakePM (n-1) xs
            ```
        6. **NOTE** If you don’t want to miss warnings on large projects, you can compile with `-error`, which causes an error anytime a warning is found.
        7. Every function must take an argument and return a result. Partial functions don’t violate this rule, but they have one significant failing. They aren’t defined on all inputs. The head function is undefined on the empty list. Nearly all errors in software are the result of partial functions. Your program receives input you don’t expect without a way to deal with it. Solution: Use the `error` function to throw an error but throwing errors easily intro bugs that compiler can't detect.
            ```
            myHead :: [a] -> a
            myHead [] = error "empty list"
            myHead (x:_) = x
            ```
        8. In practice, avoid `head` (and also `tail`) with pattern matching so the compiler can warn you of errors.
        9. Ideal: Transform partial functions to work on all values.
    2. `Maybe` for errors:
       1.  `Maybe` is a reasonable way to transform any partial function into a complete function. The below overcome the need for error function.
            ```
            maybeHead :: [a] -> Maybe a
            maybeHead [] = Nothing
            maybeHead (x:_) = Just x
            ```
        2.  `Maybe` is an instance of **Monad** (and therefore *Functor* and *Applicative*), which allows you to perform computation on values in a *Maybe context*. See unit7/lesson38/1compile.hs and [recap](https://stackoverflow.com/questions/6280585/need-to-know-what-and-do-in-haskell)
            1.  **Functor** type class allows you to use <$> to apply a function to a Maybe value.
                ```
                *Main> (+2) <$> maybeHead [1]
                Just 3
                *Main> (+2) <$> maybeHead []
                Nothing
                ```
            2. in a list structure: **Applicative** type class provides the <*> operator, so you can chain together functions in a context, most commonly used for multi-argument functions. Here’s how to use <$> with <*> to cons a result from maybeHead with a Just []. Note the **(:)** for *cons*
                ```
                *Main> (:) <$> maybeHead [1,2,3] <*> Just []
                Just [1]
                --remember
                *Main> (:) 1 []
                [1]

                *Main> (:) <$> maybeHead [] <*> Just []
                Nothing
                ```
            3. a safer function that deals with errors-causing inputs. myTakeSafer function works just fine with error-causing inputs.* tail* is also a partial function.
               ```
                myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
                myTakeSafer 0 _ = Just []
                myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                                            <*> myTakeSafer (n-1) (Just (tail xs))
               ```
    3. `Either` benefits
       1. `Maybe` has benefits like
          1. type `Int -> Maybe Bool` handles edge cases in example prime numbers ![Alt text](unit7/lesson38/isPrime.png?raw=true "Int -> Maybe Bool") <p align="center"> Maybe for edge cases </p>
       2. give expresive errors while remaining safe.
       3. 2 data constructors : *Left* and *Right*.  Left constructor as the case of having an error, and the Right constructor for when things go as planned
            ```
            data Either a b = Left a | Right b


            -- in essence
            data Either a b = Fail a | Correct b
            ```
       4. Difference **Either vs Maybe**
          1.  Left allows you to have more information than `Nothing`.
          2.  Either takes two type parameters.
       5. a type for sending error messages and a type for your actual data.
       6. Example below: Left constructor returns a String, whereas the Right constructor returns the value from the first item in your list. eitherHead :: [a] -> **Either** String a
            ```
                eitherHead :: [a] -> Either String a
                eitherHead [] = Left "There is no head because the list is empty"
                eitherHead (x:xs) = Right x

                intExample :: [Int]
                intExample = [1,2,3]
                intExampleEmpty :: [Int]
                intExampleEmpty = []

                charExample :: [Char]
                charExample = "cat"
                charExampleEmpty :: [Char]
                charExampleEmpty = ""

                *Main> eitherHead intExample
                Right 1
                *Main> eitherHead intExampleEmpty
                Left "There is no head because the list is empty"
                *Main> eitherHead charExample
                Right 'c'
                *Main> eitherHead charExampleEmpty
                Left "There is no head because the list is empty"
            ```
       7. Either type is also a member of Monad (and thus Functor and Applicative as well) and can be applied with <$>
            ```
                *Main> (+ 1) <$> (eitherHead intExample)
                Right 2
                *Main> (+ 1) <$> (eitherHead intExampleEmpty)
                Left "There is no head because the list is empty"
            ```
       8.  Either's superiority *Either type combines the safety of Maybe with the clarity that error messages provide.*
       9.  Use <$> and <*> to add the first and second numbers in intExample by using eitherHead
            ```
            (<*>) :: Applicative f => f (a -> b) -> f a -> f b
            (<$>) :: Functor f => (a -> b) -> f a -> f b
            sum2e :: Either String Int
            sum2e = (+) <$> (eitherHead intExample) <*> (eitherHead (tail intExample))
            ```
       10. take advantage of the fact that Either lets you use any type you want to, allowing you to create your own error type. **`Either` can take more than 1 `Left`s.** Here using *String* for the *Left* constructor.
            ```
                primes :: [Int]
                primes = [2,3,5,7]

                maxN :: Int
                maxN = 10

                isPrime :: Int -> Either String Bool
                isPrime n
                    | n < 2 = Left "Numbers less than 2 are not candidates for primes"
                    | n > maxN = Left "Value exceeds limits of prime checker"
                    | otherwise = Right (n `elem` primes)
            ```
       11. Create a class for errors so the Left can be other (self-created) type class
           1.  Create a type for error
                ```
                    data PrimeError = TooLarge | InvalidValue
                ```
           2. Make PrimeError an instance of Show so as to print errors.
                ```
                    instance Show PrimeError where
                    show TooLarge     = "Value exceed max bound"
                    show InvalidValue = "Value is not a valid candidate for prime checking"
                ```
           3. Refactor the isPrime function to show the errors so it looks neater now:
                ```
                    isPrime1 :: Int -> Either PrimeError Bool
                    isPrime1 n
                        | n < 2 = Left InvalidValue
                        | n > maxN = Left TooLarge
                        | otherwise = Right (n `elem` primes)

                    *Main> isPrime1 0
                    Left Value is not a valid candidate for prime checking
                    *Main> isPrime1 99
                    Left Value exceed max bound
                ```
           4. Create a displayResult function that will convert your Either response into a String. **`Either` can also have more than 1 `Right`**. Right can be first. But the Left type and Right type must align with the left and right constructor respectively.
                ```
                    displayResult :: Either PrimeError Bool -> String
                    displayResult (Right True) = "It's prime"
                    displayResult (Right False) = "It's composite"
                    displayResult (Left primeError) = show primeError --- rem data PrimeError = TooLarge | InvalidValue
                ```
           5. Create a IO
                ```
                        main :: IO ()
                        main = do
                            print "Enter a number to test for primality:"
                            n <- read <$> getLine
                            let result = isPrime1 n
                            print (displayResult result)
                ```
    4. Neither your type checker nor GHC’s warnings let you know `head` on empty list is a problem. This is ultimately caused by `head` being a partial function, a function that doesn’t return a result for all possible inputs. This can be solved by using a `Maybe` type. Although `Maybe` types do make your code safer, they can make errors hard to understand -- bcos it gives only `Nothing` for error. Finally, you saw that the `Either` type provides the best of both worlds, allowing you to safely handle errors as well as providing detailed information about them.
    5. Execises
       1. Either can accept other types beyond strings. Note alignment of left and right type in return types to Either. See unit7/lesson38/l38_1exercises.hs
            ```
                import Data.Char (isDigit)
                allDigits :: String -> Bool
                allDigits val = all (== True) (map isDigit val)

                addStrInts :: String -> String -> Either Int String
                addStrInts val1 val2
                    | allDigits val1 && allDigits val2 = Left (read val1 + read val2)
                    | not (allDigits val1 || allDigits val2) = Right "both args invalid"
                    | not (allDigits val1) = Right "first arg invalid"
                    | otherwise = Right "second arg invalid"
            ```
       2. Safe partial functions
          1. Safe Succ
            ```
                safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
                safeSucc n
                    | n == maxBound = Nothing
                    | otherwise = Just (succ n)
            ```
          2. Safe Tail
            ```
                safeTail :: [a] -> [a]
                safeTail (x :xs) = xs
                safeTail [] = []
            ```
          3. Safe Last x
            ```
                safeLast' :: Int -> [a] -> Either a String
                safeLast' 0 _ = Right "List exceeds safe bound"
                safeLast' _ (x:[]) = Left x
                safeLast' n (x:xs) = safeLast' (n - 1) xs
            ```

32. Ch 39.0 Making HTTP requests in Haskell
    1.  Steps for project
        1.  $ stack update
        2.  $ stack new http-lesson
        3.  $ cd http-lesson
        4.  $ stack setup
        5.  $ stack build
    2. Process
       1. getting results from the /datasets endpoint to get essential metadata to pass to the /data endpoint to request your data.
       2. After you’ve made the request, write the body of the request to a JSON file.
       3. First, to get your token, go to www.ncdc.noaa.gov/cdo-web/token and fill in the  form  with  your  email  address.
       4. add imports (ByteStrings. and Char8) add the Network.HTTP.Simple library, which you’ll use for your HTTP requests.
        ```
            module Main where

            import qualified Data.ByteString as B
            import qualified Data.ByteString.Char8 as BC
            import qualified Data.ByteString.Lazy as L
            import qualified Data.ByteString.Lazy.Char8 as LC
            import Network.HTTP.Simple
        ```
       5. update your http-lesson.cabal file to support these imports.
          1. add `bytestring` and `http-conduit` to your build-depends section.
          2. Because you’re working with `ByteStrings` and `Char8`, it’s also helpful to include the `OverloadedStrings` extension
          3. Changes in dependenices
             1. .cabal  ![Alt text](unit7/lesson39/dependenciesAdd.png?raw=true "Add dependencies") <p align="center"> Add dependencies </p>
             2. Alternative Or package.yaml
                ```
                default-extensions:
                - OverloadedStrings

                dependencies:
                - base >= 4.7 && < 5

                executables:
                    dependencies:
                    - http-lesson
                    - bytestring
                    - http-conduit
                ```
          4. stack will handle downloading all of your dependencies for http-conduit, and don’t need to explicitly use the stack install
       6. in Main.hs add a single API request, which will allow you to list all the data sets in the NOAA Climate Data API
          1. HTTP.Simple makes it easy for you to make simple HTTP requests.
          2. use httpLBS (the LBS stands for lazy ByteString) function to submit your request. httpLBS is able to cleverly take advantage of OverloadedStrings to make sure the correct type is passed in.
       7. To fetch data from site
            ```
            GHCi> import Network.HTTP.Simple
            GHCi> response = httpLBS "http://news.ycombinator.com"
            GHCi> response -- ALOT OF OUTPUT
            ```
       8. HTTP codes 200 OK—The request was successful.
          1. 301 Moved Permanently—The resource being requested has moved.
          2. 404 Not Found—The resource is missing.
       9.  Network.HTTP.Simple contains the function getResponseStatusCode that gives you the status of your response. `getResponseStatusCode :: Response a -> Int`
          3. Method A: With *Functor <$>* allows you to take a pure function and put it in a context.
                ```
                    GHCi> getResponseStatusCode <$> response
                    200
                    -- because
                    GHCi> :t getResponseStatusCode <$> responsegetResponseStatusCode <$> response:: Control.Monad.IO.Class.MonadIO f => f Int
                ```
          4. Method B: Alternative assign response by using <- rather than = like do-notation to allow you to treat a value in a context as though it were a pure value
                ```
                    GHCi> response <- httpLBS "http://news.ycombinator.com"
                    GHCi> getResponseStatusCode response
                    200
                ```
          5. Quick check 39.2 There’s also a getResponseHeader function. Ans : Replace `getResponseStatusCode` with `getResponseHeader` for Method A and B
       10. Your request to the API requires you to
           1. Add your token to the header.
           2. Specify the host and path for your request.
           3. Make sure you’re using the GET method for your request.
           4. Make sure your request works for an SSL connection.
       11. These 4 steaps can be done with `$` operator. `$` operator automatically wraps parentheses around your code. E.g to build HTTPS request for the API
            ```
            buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request

            buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token] -- REM TO UPDATE TOKEN
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest
            ```
       12. Each setValue function takes the argument for the parameter it’s going to set and existing request data. You start with an initial request, defaultRequest, which is provided by the Network.HTTP.Simple module. You then create a new copy of the request data with the modified parameter, finally return-ing the modified request as a result.
            ```
                setRequestMethod :: Data.ByteString.Internal.ByteString -> Request -> Request
                setRequestHeader :: http-types-0.12.3:Network.HTTP.Types.Header.HeaderName -> [Data.ByteString.Internal.ByteString] -> Request -> Request
            ```
       13. Putting it together
           1.  pass your request into httpLBS.
           2.  get the status
           3.  if 200: you’ll write the data to a file by using the getResponseBody function.
           4.  otherwise : alert the user that there was an error in your request.
           5.  write file using raw lazy ByteStrings with L.writeFile
           6.  Never write it using the Char8 interface, as it can corrupt your data.
       14. a basic application that can fetch data from the REST API and write it to a data.json file. See in unit7/lesson39/http-lesson/app/Main.hs
           1.  buildRequest / buildReuqestNoSSL
           2.  request      / requestNoSSL
           3.  main
33. Ch40.0 Working with JSON data by using AESON
    1.  JSON data example ![Alt text](unit7/lesson40/json.png?raw=true "JSON data") <p align="center"> JSON data </p>
    2. Process of transforming objects into and out of JSON is known as serialization anddeserialization, respectively.
        ```
        data User = User
            { userId :: Int
            , userName :: T.Text
            , email :: T.Text
            }
        ```
    3. The key challenge of working with JSON in Haskell is that JSON supports only a few simple types: objects, strings, numbers (technically just floats), Booleans, and lists.
       1. Aeson allows you to translate back and forth between Haskell’s powerful data types and JSON.
       2. Aeson relies on two key functions for translating back and forth between Haskell data types and JSON: encode and decode.  To use these two functions, you need to make your data an instance of two type classes: ToJSON (encode) and FromJSON (decode). Two ways to do this:
          1. automatically deriving the type classes with the help of a language extension.
          2. implement these classes yourself.
    4. Stack project `json-lesson` with all codes in `Main` module with:
       1. the popular Aeson library for working with JSON.
       2. form of Data.Text, because this is the preferred method in Haskell for representing text.
       3. import lazy ByteStrings and the Char8 helper for these.
       4. JSON will be represented as ByteStrings by default until you transform it
       5. See unit7/lesson40/json-lesson/app/Main.hs for imports
       6. delete package.yaml and add libraries to cabal (NOPE). (change package.yaml INSTEAD at default-extensions & executables-dependencies)
    5. Aim of Aeson is to convert back and forth between Haskell data types and raw JSON. Aeson two key functions
       1. `decode` function takes JSON data and transforms it into a target type. `Maybe` is a good way to handle errors in Haskell since concerned with are parsing the JSON data correctly
            ```
            decode :: FromJSON a => ByteString -> Maybe a
            ```
       2. `Either` is often a better type because it can tell you what went wrong. Aeson also offers an `eitherDecode` function that will give you more informative error messages by using the Left constructor (remember that Left is the constructor used for errors)
            ```
            eitherDecode :: FromJSON a => ByteString -> Either String a
            ```
       3. To use `eitherDecode`, I must first make a type an instance of FromJSON to enable conversion from raw JSON into a Maybe / Eitherinstance of your type. You’ll explore ways of making data an instance of FromJSON.
       4. `encode` function takes a type that’s an instance of `ToJSON` and returns a JSON object represented as a ByteString. `ToJSON` is the counterpart to FromJSON. If a type is an instance of both `FromJSON` and `ToJSON`, it can trivially be converted to and from JSON.
            ```
            encode :: ToJSON a => a -> ByteString
            ```
        1. Aeson uses two type classes: **FromJSON** and **ToJSON**. The FromJSON type class allows you to parse JSON and turn it into a Haskell data type, and ToJSON allows you to turn Haskell data types into JSON.
    6. **DeriveGeneric** -- a language extension that makes it possible to easily derive instances of FromJSON and ToJSON simply add Generic to your *deriving* statement. See unit7/lesson40/json-lesson/app/Main.hs
       1. make the Book type both an instance of FromJSON and ToJSON. ![Alt text](unit7/lesson40/deriveGeneric.png?raw=true "Making a FromJSON and ToJSON instance") <p align="center"> Making a FromJSON and ToJSON instance </p> and see the results:
        ```
            1. stack ghci
            2. *Main Lib Paths_json_lesson> myBook
            Book {title = "Learn Haskell", author = "Will Kurt", year = 2017}
            *Main Lib Paths_json_lesson> myBookJSON
            "{\"year\":2017,\"author\":\"Will Kurt\",\"title\":\"Learn Haskell\"}"

        ```
       2. From a string of JSON with littyle type info to a Haskell type. ![Alt text](unit7/lesson40/deriveGeneric1.png?raw=true " FromJSON to Haskell Type") <p align="center"> Making a FromJSON to Haskell Type </p>
          ```
          *Main Lib Paths_json_lesson> bookFromJSON
          Just (Book {title = "A Short History of Decay", author = "Emil Ciroan", year = 1949})
          ```
    7. Better parse error message with either. Run `eitherDecode wrongJSON :: Either String Book`
         ```
         GHCi> bookFromWrongJSON
         Nothing
         GHCi> eitherDecode wrongJSON :: Either String Book
         Left "Error in $: The key \"author\" was not found"
         ```
    8.  Working with JSON from others. See Section 4 in unit7/lesson40/json-lesson/app/Main.hs
        1.  Define Error Message. Note to use errorCode instead of error
            ```
                data ErrorMessage = ErrorMessage
                                    { message :: T.Text
                                    , errorCode :: Int
                                    } deriving Show

                sampleError :: BC.ByteString
                sampleError = "{\"message\":\"oops!\",\"error\": 123}"

                exampleMessage :: Maybe T.Text
                exampleMessage = Just "Opps"
                exampleError :: Maybe Int
                exampleError = Just 123
            ```
        2. to make your ErrorMessage type an instance of FromJSON, you need to define one function: **parseJSON**. make ErrorMessage in the context of Maybe
            ```
                instance FromJSON ErrorMessage where
                    parseJSON (Object v) =                -- (Object v) is the JSON object being parsed
                        ErrorMessage <$> v .: "message"   -- ErrorMessage <$> value <*> value
                                    <*> v .: "error"
            ```
        3. combine <$> and <*> to safely make this ErrorMessage in the context of a Maybe
            ```
                *Main> ErrorMessage <$> exampleMessage <*> exampleError
                Just (ErrorMessage {message = "Opps", errorCode = 123})
            ```
        4. **(.:)** This operator takes an Object (your JSON object) and some text and returns a value parsed into a context. `(.:) :: FromJSON a => Object -> Text -> Parser a` . so `v .: "message"` results in a value in a Parser context.  We need a context for your parse so that it can fail if there’s trouble parsing
        5. Now ErrorMessage type is an instance of FromJSON, you can finally parse the incoming JSON ErrorMessages.
    9. Working toJSON. See Section 4 in unit7/lesson40/json-lesson/app/Main.hs ![Alt text](unit7/lesson40/toJSONinstance.png?raw=true " ToJSON") <p align="center"> ToJSON </p>
       1. takes your data constructor and pattern matches on its two arguments, message and errorCode:
            ```
                instance ToJSON ErrorMessage where
                toJSON (ErrorMessage message errorCode) =
                    object [ "message" .= message
                            , "error" .= errorCode
                        ]
            ```
       2. the method takes your data constructor and pattern matches on its two arguments, message and errorCode: `toJSON (ErrorMessage message errorCode)`
       3.  then use the object function to create your JSON object, passing the values of your data type into the correct fields for the JSON object:
       4.  another new operator here, (.=). This operator is used to create a key/value pair matching the value of your data with the field name for the JSON object.
   10.  Lesson 39 output: to model the entire response with a NOAAResponse data type. NOAAResponse is made up of two types: Metadata and Results. Metadata itself contains another type, Resultset. Then you have NOAAResults, which contains values. The JSON from the NOAA has a nested structure.
        1.   Because Result contains an id value, you need to define a custom implementation of your instances - resultID. Because the data uses id instead of resultId, **you need to make your own instance of FromJSON. You’re not concerned about ToJSON, because you’ll be reading only from the data.** ![Alt text](unit7/lesson40/fromJSONinstance.png?raw=true " FromJSON") <p align="center"> FromJSON </p>
        2.   Metadata
             1. first part of your Metadata is Resultset. Define your type, add deriving (Generic), and make it an instance of your type class
                ```
                    data Resultset = Resultset
                            { offset :: Int
                            , count :: Int
                            , limit :: Int
                            } deriving (Show,Generic)

                instance FromJSON Resultset
                ```
             2. Metadata data type itself has only the Resultset value
                ```
                    newtype Metadata = Metadata
                                {
                                  resultset :: Resultset
                                } deriving (Show,Generic)

                    instance FromJSON Metadata
                ```
        3. put together these other types into your NOAAResponse.
            ```
                data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

                instance FromJSON NOAAResponse
            ```
        4. print out all the types in the  file with a `printResults` IO action. Include a error case message. use forM_from the Control.Monad module (remember to import Control.Monad) to loop through your results and print them. The `forM_` function works just like the `mapM_` function, only it reverses the order of the data and the function used to map over the data
            ```
                printResults :: Maybe [NOAAResult] -> IO ()
                printResults Nothing = print "error loading data"
                printResults (Just results) =  do
                            forM_ results (print . name)
                            -- print DataName -- something wrong

                main :: IO ()
                main = do
                    jsonData <- B.readFile "data.json"-- "../lesson39/http-lesson/data.json"
                    let noaaResponse = decode jsonData :: Maybe NOAAResponse
                    let noaaResults = results <$> noaaResponse
                    printResults noaaResults
            ```
   11. Summary:
        1.  the popular Aeson library, which makes it possible to convert back and forth between Haskell data types and JSON
        2.   conversion between data types and JSON is achieved with two type classes: FromJSON and ToJSON.
        3.   best case, you can use the DeriveGeneric language extension to derive these classes automatically then do `deriving (Show, Generic)` and add `instance FromJSON or ToJson NameOfDataType`
        4.   worst case, where you have to help Aeson translate your data types, is still easy.
   12. Q40.1 Make    your    NOAAResponse type an instance of ToJSON. See unit7/lesson40/MainFromJSON.hs
            ```
                instance ToJSON NOAAResponse where
                    toJSON (NOAAResponse uid mindate maxdate name datacoverage resultId) =
                        object  [ "uid"      .= uid
                                , "mindate"  .= mindate
                                , "maxdate"  .= maxdate
                                , "name"     .= name
                                , "datacoverage" .= datacoverage
                                , "id"        .= resultId
                                ]
                instance ToJSON Resultset
                instance ToJSON Metadata
                instance ToJSON NOAAResponse
            ```
   13. Q40.2 Make a Sum type called IntList and use DerivingGeneric to make it an instance of ToJSON. Don’t use the existing List type, but rather write it from scratch. Here’s an example of an IntList:
            ```
                intListExample :: IntList
                intListExample = Cons 1 $
                                 Cons 2 EmptyList
                data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)
                instance ToJSON IntList
                instance FromJSON IntList
            ```