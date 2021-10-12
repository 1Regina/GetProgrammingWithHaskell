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
           1.  Solution 1: **option 1** go straight to **package.yaml**#dependencies and add instead then do `stack build` after [listing dependencies](https://docs.haskellstack.org/en/stable/GUIDE/#:~:text=between%20different%20runs.-,Adding%20dependencies,-Let%27s%20say%20we) . e.g to add text. (demo-ed: unit6/lesson35/palindrome-checker1). Note for [curated packages](https://docs.haskellstack.org/en/stable/GUIDE/#:~:text=in%20using%20stack.-,curated%20package%20sets) add in `extra-deps` field in `stack.yaml` to define extra dependencies not present in the resolver.
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
        2. Metadata
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
    12. Q40.1 Make your NOAAResponse type an instance of ToJSON. See unit7/lesson40/MainFromJSON.hs

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
34. Ch41.0 USING DATABASES IN HASKELL
    1.  Database tasks via the `sqlite-simple` library
        1.  Create - Add new data to the database
        2.  Read - Querying the database for into
        3.  Update - Modifying existing data
        4.  Delete - Removing data from the database
    2. Build new project 'db-lesson' on three tables: tools, users, and checkedout with changes .cabal (see book) or in package.yaml (my way)
       1. package.yaml
            ```
                default-extensions:
                - OverloadedStrings

                executables:
                    db-lesson-exe:
                        dependencies:
                        - db-lesson
                        - time
                        - sqlite-simple
            ```
       2. Main.hs  ![Alt text](unit7/lesson41/imports.png?raw=true "Main.hs imports") <p align="center">imports for Main.hs</p>
            ```
                module Main where

                import Control.Applicative
                import Database.SQLite.Simple
                import Database.SQLite.Simple.FromRow
                import Data.Time

                main :: IO ()
                main = print "db-lesson"
            ```

       3.  `OverloadedStrings` extension, because many of your strings will be inter-preted as SQL queries by the SQLite Simple library
       4.  Install SQLite from www.sqlite.org.
       5.  Create a db with unit7/lesson41/db-lesson/build_db.sql in the root directory of `db-lesson` project and in the root directory run sqlite3 with **`sqlite3 tools.db < build_db.sql`**
       6.  Activate interactive sqlite with `sqlite3 tools.db`
       7.  With sqlite prompt, test to see the db creation is ok with
           1. `select * from tools;`
           2. `select * from users;`
       8. Haskell db from now on...
       9. Put Tool into Main.hs
            ```
                data Tool = Tool
                    { toolId :: Int
                    , name :: String
                    , description :: String
                    , lastReturned :: Day
                    , timesBorrowed :: Int
                    }
            ```
       10. To test some functions from (Date.Time) module imports dependencies in the project/root directory, do
           1.  `stack ghci` to get into interactive ghci
           2.  Get current day into `Day` type that is used in `Tool` data type in Main.hs by transforming it into a Day type by using **utctDay**
                ```
                        *Main Lib Paths_db_lesson> getCurrentTime
                        2021-09-08 13:19:55.854874 UTC
                        *Main Lib Paths_db_lesson> utctDay <$> getCurrentTime
                        2021-09-08
                ```
       11. Put User into Main.hs
            ```
                data User = User
                    { userId :: Int
                    , userName :: String
                    }
            ```
       12. Add show instance by `instance Show ...`
           1.  User
                ```
                    instance Show User where
                        show user = mconcat [ show $ userId user
                                            , ".)  "
                                            , userName user]
                ```
           2.  Tool
                ```
                    instance Show Tool where
                        show tool = mconcat [ show $ toolId tool
                                            , ".) "
                                            , name tool
                                            , "\n description: "
                                            , description tool
                                            , "\n last returned: "
                                            , show $ lastReturned tool
                                            , "\n times borrowed: "
                                            , show $ timesBorrowed tool
                                            , "\n"]
                ```
       13. Then `stack run` in terminal to load module, and run per Data type with `User 1 "regina"`
       14. Quick check 41.1    Why    is    mconcat preferred for combining your strings over ++?
            ```
            Becos mconcat makes your code easier to refactor with text type. Although you used String in this lesson, you often would prefer touse  the  Text  type.  mconcat  works  on  all  major  string  types:  String, Text,  and  ByteString.  This  makesrefactoring code to change types as easy as changing type signatures.
            ```
    3. Creating data in Haskell: To insert data into your database, you need to connect to the database, create a SQL string, and then execute the SQL
       1. establish a connection to database.
       2. **execute** to insert users into database via a function that takes a userName and inserts it into your database.
       3.  a query string that contain a (?), which allows you to safely pass values into your string.
       4.  addUser code. **Only** constructor is used to create single-element tuples. ![Alt text](unit7/lesson41/dbConnectionAction.png?raw=true "Action connected to database") <p align="center">Action connected to database</p>
       5. `withConn` to abract out connecting action to db.
            ```
                withConn :: String -> (Connection -> IO()) -> IO()
                withConn dbName action = do
                        conn <- open dbName
                        action conn
                        close conn

            ```
       6. a checkout function. Notice that (userId,toolId) is a plain tuple with no Only data constructor needed.
            ```
                checkout :: Int -> Int -> IO ()
                checkout userId toolId = withConn "tools.db" $
                                        \conn -> do
                                                execute conn
                                                "INSERT INTO checkout (user_id, tool_id) VALUES (?, ?)"
                                                (user_id, tool_id)

            ```
       7. The challenge when working with SQL data in Haskell is that you need an easy way to make instances of Haskell data types from raw data. To achieve this, the sqlite-simplelibrary includes a type class called `FromRow`.
          1. The `fromRow` method returns a `RowParser` of type a, where a is the same type as whatever type you’re making an instance of `FromRow`. `FromRow` is used by functions to query your data by easily transform queries into lists of your data types
          2.  a function from `SQLite.Simple` called `field`. The `field` function is used internally by SQLite.Simple to consume the data from a rowin db and transform it into the values used by your type constructors. With unit7/lesson41/db-lesson/app/Main.hs, you can make queries against your database and translate them directly into lists of users and tools.
          3. `query` and `query_` to query data. The underscore version takes one less argument. The query function assumes that you’re passing in a query string and parameter for that query. The query_ function with an underscore is for queries that take queries with no parameters as arguments.
                ```
                    query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
                    query_ :: FromRow r => Connection -> Query -> IO [r]

                ```
          4. notice that Query is its own type. You’ve been treating your queries as strings, but this is all thanks to the Overloaded-Strings extension, which is automatically translating for you.
          5. Quick check 41.3     Why do you need two functions, query and query_? *Answer* Primarily because of the way Haskell handles types, Haskell doesn’t support variable arguments. An alternative to making two functions is to use a Sum type that represents both sets ofarguments and use pattern matching.
          6. printToolQuery function that takes a query and prints out the tools returned by that query
    4. Updating existing data.
       1. When a tool is checked back in, you want to do two updates. First, you want to increment its existing timesBorrowed by 1; second, you want to update the lastReturned date to the current date.
          1. `selectTool` function takes a `connect` and a `toolId` to look up the tool. `firstOrNothing` function looks at the list of results returned by your query. If the list is empty, it returns Nothing; if you have results (presumably just one, because the ID is unique), it returns the first one.
          2. `updateTool` function takes an existing tool and returns a new tool with an updated lastReturned date and timesBorrowed count, using record syntax (lesson 11).
          3. `updateToolTable`, takes a `toolId`, fetches the current date, and then performs the necessary steps to update the tool in the table

    5. Delete data with `execute`.
       1. `checkin` action takes a `toolID` and deletes the row from the checkedout table.
       2. Or check and update the database with `checkinAndUpdate`.
    6. An interface with `promptAndAddUser`, `promptAndCheckout` and `promptAndCheckin` and `performCommand` with the >> operator (which performs an action, throws away the result, and performs the next) to call main. This allows your command-line interface to repeatedly prompt the user for more input until the user quits your program
       1. Quick check 41.4     Why can’t you use >>= instead of >>?
       2. Answer: When you use >>=, you’re passing an argument in a context; the >> operator is used when you want to chain together actions and disregard their output
    7. A minimal main to do `performCommand command`.
    8. `performCommand` calls `main`, and `main` executes the `performCommand` action, leading to code that’s recursive normally **BUT** Haskell will notice that each function calls the other last, and is able to optimize this safely.
       1.  build your program `stack build` and run with `stack exec db-lesson-exe`
       2.  possible commands when prompted
           1.  users
           2.  tools
           3.  checkout
           4.  in
           5.  out
           6.  checkin
           7.  addtool (failing) q1 **ERROR**
       3.  `quit` to terminate
35. Ch42.0 Efficient, stateful arrays
    1.  Bubble sort: List are inefficient and lazy evaluation cause major performance.`:set +s` to time function in GHCI
        1.  Lists take time (See unit7/lesson42/1uarray.hs)
            ```
                aLargeList :: [Int]
                aLargeList = [1 .. 10000000]

                GHCi> :set +s
                GHCi> [1 .. 10000000] !! 9999999
            ```
        2. `UArray` lookup operator `!` is faster
           ```
                aLargeArray :: UArray Int Int
                aLargeArray = array (0,9999999) []

                GHCi> aLargeArray ! 9999999
                0
                (0.00 secs, 456,024 bytes)
           ```
        3. The U in UArray stands for unboxed. Unboxed arrays don’t use lazy evaluation (they use strict evaluation). Lazy evaluation, although powerful, is another frequent cause of inefficiency. Lazy evaluation = no computation until required which includes generating the list in the first place. The catch with using **unboxed arrays** is that they *work only with primitive types*: **Int, Char, Bool, and Double**. Haskell does offer a more general Array type that will work with any data the same way that List does, but Array is a lazy data structure.
    2.  To use the UArray type, you’ll **import Data.Array.Unboxed** to the top of your module. Additionally, if using stack, you need to **add array to the list of build-depends**.
        1.  `UArrays` take two type parameters;
            1.  the first is for the type of the index -  members of `Enum` and `Bounded` ie `Char` or `Int`, but not `Double` (not an instance of `Enum`), and not `Integer` (not an instance of `Bounded`). `Bool` type index is ok.
            2.  and the second is for the type of the value.
        2.  Create a `UArray` with `array` function which takes 2 arguments:
            1.  The first is a pair of values in a tuple representing your lower and upper bounds for your indices.
            2.  The second argument is a list of (index, value) pairs
        3. If you’re missing an index in your pairs, Haskell will provide a default value. **For Int types, this is 0; and for Bools, it’s False.**
           1. Sample: zero-indexed array of Bools.
                ```
                    zeroIndexArray :: UArray Int Bool
                    zeroIndexArray = array (0,9) [(3,True)]

                    -- so all unset values are false
                    -  look up values in your UArray by using the ! operator

                    GHCi> zeroIndexArray ! 5
                    False
                    GHCi> zeroIndexArray ! 3
                    True
                ```
           2. Sample: 1-indexed array with all the Bools set to True. You’ll use your zip function combined with cycle to generate a list of value pairs that are all True
                ```
                    oneIndexArray :: UArray Int Bool
                    oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]
                ```
           3. Quick check 1: Create an array of five elements indexed at 0 and the 2, and three elements are set to True
                ```
                    qcArray :: UArray Int Bool
                    qcArray = array (0,4) $ zip [0 .. 2] $ cycle [True]
                    qcArray' :: UArray Int Bool
                    qcArray' = array (0,4) [(1,True),(2,True)]
                ```
           4. look up values in your UArray by using the **! operator** if you try to access an element outside your index bounds, you’ll get an error
        4. 42.1.3 UArray can be updated like any functional data structure.
           1. Case Example to add five beans to bucket 1 and six beans to bucket 3 (with bucket 0 being your first bucket) by using the (//) operator. The first argument to (//) is UArray, second argument is a new list of pairs. The result is a new UArray with the updated values.
                ```
                    updatedBiB :: UArray Int Int
                    updatedBiB = beansInBuckets // [(1,5),(3,6)]
                               = array (0,3) [(0,0),(1,5),(2,0),(3,6)]

                ```
            2. Now to add two beans to every bucket. Use the `accum` function which takes first: a binary function, second: a UArray, and third: a list of values to apply the function to.
                ```
                    accum (+) updatedBiB $ zip [0 .. 3] $ cycle [2] = array (0,3) [(0,2),(1,7),(2,2),(3,8)]
                ```
            3. Doubling the number of beans in each bucket.
                ```
                    accum (*) updatedBiB $ zip [0..3] $ cycle [2] =  array (0,3) [(0,0),(1,10),(2,0),(3,12)]
                ```
    3.  Summary from above examples: With UArray, you can get efficient lookups as well as a more efficient data structure. When you used `UArray`, you were able to replicate an artificial sense of mutable state. But when you’re using state specifically for efficiency reasons, this is a terrible solution. Haskell can remove statefulnsss from code to get safer, more predictable code with roughly the same performance. But Haskell can also mutate state --  using a special type of `UArray` called an `STUArray`. The `STUArray` uses a more general type called `ST`. `ST` allows for stateful, nonlazy programming. Focus here is `STUArray`. For stateful, nonlazy programming (See unit7/lesson42/2stateful.hs)
        1.  use `STUArray` by adding the following imports:
            ```
                import Data.Array.ST
                import Control.Monad
                import Control.Monad.ST
                -- import Data.Array.Unboxed -- to use UArray also
            ```
        2. `STUArray `is an instance of `Monad` to perform arbitrary computation within a context. 4 other contexts learnt so far:
           1. `Maybe` types model the context of missing values.
           2. `List` types can be used to represent the context of nondeterministic computing.
           3. `IO` types allow you to separate stateful, error-prone I/O code from your pure functions.
           4. `Either` types provide a more detailed way to handle errors over `Maybe` type
        3. use do-notation to treat types in the **`STUArray` context**, just as if they were regular data.
        4. Key power `STUArray` offers is the ability to change values in a `UArray`.  `STUArray` exists to allow you to perform stateful programming only when that statefulness is indistinguishable from pure code for the users of your functions.
           1. By being able to change values in place, you can save tremendously on *memory usage*,
           2. and by not having to re-create a new copy of your data structure with each change, you can also *save on time*.
           3. STUArray is a context that allows for stateful mutations.![Alt text](unit7/lesson42/stuArrayContext.png?raw=true "STUArray context") <p align="center">STUArray is a context that allows for stateful mutations</p>
    4. **listToArray**: use `STUArray` by writing a function, called `listToSTUArray`, that takes a list of Ints and transforms that into an `STUArray`.
        1. first draft is iitializing an empty array of a fixed size ... *in a monad* .
        2. `STUArray` type uses the `newArray` function, which takes a pair representing the bounds of the array as well as a value for initializing the array.
        3. (See unit7/lesson42/2stateful.hs).  ![Alt text](unit7/lesson42/doNotation.png?raw=true "listToSTUArray Step 1 - put in context") <p align="center">First sketch to listToSTUArray function - put in context</p>
        4.  add your loop, which you’ll run through your list, and update your `stArray` value.  use `forM_ `from C`ontrol.Monad`. The **`forM_`** action takes your data and a function to apply to the data as arguments. This has the nice property of replicating a *for in* loop in languages such as Python. Validate by using a list of indices and (!!) to look up values in your list.
              ```
                  listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
                  listToSTUArray vals = do
                      let end =  length vals - 1
                      myArray <- newArray (0,end) 0
                      forM_ [0 .. end] $ \i -> do
                          let val = vals !! i
                          writeArray myArray i val
                      return myArray
              ```
        5.  While it is more efficient to zip indices with list values, for a more stateful language *feel*, write the values from the list to your `stArray` value. For this, use the `writeArray` function, which takes an `STUArray`, an index, and your value. `writeArray` function performs a stateful mutation of your underlying array and doesn’t create a copy of it.![Alt text](unit7/lesson42/copyListTostuArray.png?raw=true "listToSTUArray Step 2 - rewrite data in array") <p align="center">Second step to listToSTUArray function - rewrite data in array</p>
        6.  Problem, unlike IO context when we load the module in GHCI, we cant read the output.
              ```
                  *Main> listToSTUArray [1,2,3]
                  <<ST action>>
              ```
        7.  Solution to read values out of a context when you’re using `STUArray`.  Although `STUArray` is similar to IO, you’re dealing with a much safer context. In `listToSTUArray` code, because statefulness is contained in a context, you’re forced to make sure your stateful code obeys encapsulation. Encapsulation means that objects properly hide all of their implementation details from the user. Because `STUArray` is enforcing encapsulation, you’re not constrained by the same limitations of IO. You can take values out of `STUArray` by using a function named `runSTUArray`.
              ```
                  runSTUArray :: ST s (STUArray s i e) -> UArray i
                  listToUArray :: [Int] -> UArray Int Int
                  listToUArray vals = runSTUArray $ listToSTUArray vals
                  -- >>> listToUArray [1,2,3]
                  -- array (0,2) [(0,1),(1,2),(2,3)]
              ```

        8. visualized in figure 42.2 ![Alt text](unit7/lesson42/takeSTUArrayvalues.png?raw=true "listToSTUArray Step 3 - take values out of context") <p align="center">Third step to listToSTUArray function - take values out of context</p>
        9. Step 4: Common Haskell without intermediary function (listToSTUArray)
              ```
                  listToUArrayGold :: [Int] -> UArray Int Int
                  listToUArrayGold vals = runSTUArray $ do
                      let end =  length vals - 1
                      myArray <- newArray (0,end) 0
                      forM_ [0 .. end] $ \i -> do
                          let val = vals !! i
                          writeArray myArray i val
                      return myArray
                  -- >>> listToUArrayGold [1,2,3]
                  -- array (0,2) [(0,1),(1,2),(2,3)]
              ```
    5. Extra. Just as with `STUArray`, the primary purpose of **all ST types** is to allow you to implement perfectly encapsulated, stateful computation. The `ST type` generalizes the behavior you see in `STUArray`. The `STUArray` type relies primarily on three actions: `newArray`, `readArray`, and `writeArray`. For the ST type, these are replaced with the more general functions: `newSTRef`, `readSTRef`, and `writeSTRef`. Likewise, instead of `runSTUArray`,  you  use  `runST`.  Here’s  a  simple  example  of  a  `swapST`  function  that  statefully swaps the values of two variables in a 2-tuple:
         ```
             swapST :: (Int,Int) -> (Int,Int)
             swapST (x,y) = runST $ do
                 x' <- newSTRef x
                 y' <- newSTRef y
                 writeSTRef x' y
                 writeSTRef y' x
                 xfinal <- readSTRef x'
                 yfinal <- readSTRef y'
                 return (xfinal,yfinal)
         ```
    6. Bubble sort at unit7/lesson42/3bubbleSort.hs.
       1. An imperative algorithm implemented in Haskell.
            ```
                myData :: UArray Int Int
                myData = listArray (0,5) [7,6,4,8,10,2]
                bubbleSort :: UArray Int Int -> UArray Int Int
                bubbleSort myArray = runSTUArray $ do
                    stArray <- thaw myArray
                    let end = (snd . bounds) myArray
                    forM_ [1 .. end] $ \i -> do
                        forM_ [0 .. (end - i)] $ \j -> do
                            val <- readArray stArray j
                            nextVal <- readArray stArray (j + 1)
                            let outOfOrder = val > nextVal
                            when outOfOrder $ do
                                writeArray stArray j nextVal
                                writeArray stArray (j + 1) val
                    return stArray

                -- >>> bubbleSort myData
                -- array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
            ```
       2. Implement bubble sort: ![Alt text](unit7/lesson42/bubbleSort.png?raw=true "Bubble Sort Implementation") <p align="center">Bubble Sort Implementation</p>
    7. Exercises in unit7/lesson42/l42exercises.hs
       1. Q1: One of the most important operations in the implementation of a genetic algo-rithm is combining two arrays of Booleans through an operation called crossover. Cross-over takes as input a pair of equal-sized arrays. Then a cutoff point is chosen, and the top and bottom are swapped. The final value is this new pair of arrays. Here’s an illustration using lists and an example (using 1 for True and 0 for False): ([1,1,1,1,1],[0,0,0,0,0]). If you perform crossover at index 3, your result should be [1,1,1,0,0]. Implement crossover where the result is a UArray but the crossover itself is performed using STUArrays.
            ```
                crossOver :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
                crossOver (array1,array2) crossOverPt  = runSTUArray $ do
                    stArray1 <- thaw array1
                    let end = (snd . bounds) array1
                    forM_ [crossOverPt .. end] $ \i -> do
                        writeArray stArray1 i $ array2 ! i
                    return stArray1

                array1 :: UArray Int Int
                array1 = array (0,4) $ zip [0 .. 4] $ cycle [1]
                -- >>> array1
                -- array (0,4) [(0,1),(1,1),(2,1),(3,1),(4,1)]

                array2 :: UArray Int Int
                array2 = array (0,4) $ zip [0 .. 4] $ cycle [0]
                -- >>> array2
                -- array (0,4) [(0,0),(1,0),(2,0),(3,0),(4,0)]

                *Main> crossOver (array1, array2) 3
                array (0,4) [(0,1),(1,1),(2,1),(3,0),(4,0)]
            ```
       2. Q2: Write a function that takes a `UArray Int Int` as an input. The input will have a mixture of zeros and other values. The function, `replaceZeros`, should return the array with all of the zeros replaced with the value –1.
            ```
                rawArray :: UArray Int Int
                rawArray = array (0,5) [(0,0),(1,3),(2,0),(3,9),(4,4),(5,0)]

                replaceZeroes :: UArray Int Int -> UArray Int Int
                replaceZeroes array = runSTUArray $ do
                    stArray <- thaw array
                    let end = (snd . bounds) array
                    forM_ [0 .. end] $ \i -> do
                        val <- readArray stArray i
                        when (val == 0) $ do
                            writeArray stArray i (-1)
                    return stArray

                *Main> replaceZeroes rawArray
                array (0,5) [(0,-1),(1,3),(2,-1),(3,9),(4,4),(5,-1)]
            ```
