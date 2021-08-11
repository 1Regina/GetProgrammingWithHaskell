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
       5.  A. Add edits to your **.cabal** file to tell stack about any modules you’re depending on e.g. `Data.Text` into the `build-depends:` ![Alt text](unit6/lesson35/cabalEdits.png?raw=true "Add packages to dependencies") <p align="center"> Additions to build dependencies </p>
       6.  A. With everything together to build project, ensures that stack is using the correct version of GHC, run setup **in project directory** with:
           ```
           stack setup
           ```
       7.  A. Ensuring that your project is built with the version of GHC you used to write it is important. Specifying the version of GHC you want to use is done indirectly by choosing your stack resolver version. The stack resolver is set in the **stack.yaml** file: `resolver: lts-7.9`. The lts-7.9 version of the stack resolver uses GHC version 8.0.1. By default, stack uses the most recent stable resolver. Listing of the current resolver versions at www.stackage.org. For info on specific resolver, e.g 7.9 by www.stackage.org/lts-7.9 for the lts-7.9 resolver)
       8. A. Build your project with `stack build`.
       9. A. Run the project  with `exec` as in `stack exec palindrone-checker-exe` using the default 's `<project-name>-exe` The name of the project is taken from .cabal file line #38 `executable palindrone-checker-exe`
       10.  A. **QuickFix** For large program with `Data.Text`, add OverloadedStrings pragma to every file. Shortcut:
           1.  Go to .cabal file and universally apply the `OverloadString` language extension at after default-language:
           2.  after default-language:  Haskell2010 to **both** your library and executable sections of .cabal:
               ```
               extensions: OverloadedStrings
               ```
       11.  B **OTHERWISE** if the [.cabal#line3](https://github.com/1regina/GetProgrammingWithHaskell/blob/master/unit6/lesson35/palindrome-checker/palindrome-checker.cabal#L3) is `-- This file has been generated from package.yaml by hpack version 0.34.4.`, then
           1.  **option 1** go straight to **package.yaml**#dependencies and add instead . e.g to add text. (demo-ed: unit6/lesson35/palindrome-checker1)
               ```
               dependencies:
               - base >= 4.7 && < 5
               - text
               ```
           2. **option 2** delete `package.yaml` file and edit the `.cabal` file `build-dependencies` for required sections (demo-ed: unit6/lesson35/palindrome-checker1WOPackageYaml)
    3.  then do `stack run`
    4.  Difference between palindrome-checker vs palindrome-checker1 is src directory. Both have package.yaml files

        | palindrome-checker                                                               | palindrome-checker1                                                                     |
        | -------------                                                                    | -------------                                                                           |
        | src directory only has Lib.hs                                                    | src directory has Lib.hs and Palindrome.hs                                              |
        | src/Lib.hs has all the preprocessing (stripWhiteSpace, stripPunctuation etc) fns | src/Palindrome.hs has all the preprocessing (stripWhiteSpace, stripPunctuation etc) fns |
        | app/Main.hs import Lib module of src/Lib.hs                                      | app/Main.hs import Palindrome module of src/Palindrome.hs                               |
    5.  Exercises.
        1.  Q35.1: (unit6/lesson35/palindrome-checker1) Cant do `extensions: OverloadedStrings` in .cabal nor package.yaml alternative (see pt 10 above under QuickFix) so retain OverloadedStrings pragma in Main.hs and Palindrome.hs. Completed below and `stack run`
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
        3.  write a function to express this property by import Data.Char (isPunctuation) and put this function in your Spec.hs file.
        4. Import *Data.Char (isPunctuation)* and put the property function into test/Spec.hs ![Alt text](unit6/lesson36/propertyFunction.png?raw=true "Test a property in a function") <p align="center"> Test a property in a function </p>
        5. See property testing 1: functions in src/Lib.hs after `import Data.Char (isPunctuation)`
            ```
            prop_punctuationInvariant text = preprocess text ==
                                            preprocess noPuncText
                where noPuncText = filter (not. isPunctuation) text

            ```
        6. Property testing 2: function in src/Lib.hs
            ```
            prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)
            ```
        7. But step 5 and 6 do not do the property testing.
    9.  Enter **QuickCheck** for property testing how: you supply properties that your code is supposed to uphold, and then QuickCheck automatically generates values and tests them on the functions, making sure the properties are upheld.
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
        7. **continue with unit6/lesson36/palindrome-testingWOPackageYaml_String (ie good old .cabal) henceforth to avoid duplicate notes documentation** Without `import Data.Char(isPunctuation)` in Lib.hs (ORIGINAL without stack install quickcheck-instances):
            ```
            import Data.Char (isPunctuation)

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
           1.  **Remedy to expand types covered by QuickCheck:** `stack install quickcheck-instances` to installs a new package to the project. Now can handle Data.Text beyond String type.
           2.  Next is refactor
               1. Refactor src/Lib.hs replacing String with T.Text and  filter, reverse with T.filter and T.reverse
               2. Refactor test/Spec.hs
                    ```
                        -- after stack install quickcheck-instances ie can handle Data.Text
                        import Test.QuickCheck.Instances
                        import Data.Char(isPunctuation)
                        import Data.Text as T
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
    12.   another benefit of property testing. Just refactor and do `stack test`
    13.   Summary Things done/learnt:
          1. manually testing your code by using stack ghci to ensure code build as expected.
          2. stack test command to build and run a series of simple unit tests
          3. generalized your unit testing by creating property tests with QuickCheck.
