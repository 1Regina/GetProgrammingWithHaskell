28. Ch34.0 ORGANIZING HASKELL CODE WITH MODULES
    1. You  have  a  type  for  books  and  a  type  for  magazines.  Each  has  thesame field names, but they represent different things. Both types are written using record syntax, which creates a problem. Record syntax automatically creates accessor functions title and price. Unfortunately, this causes an error because you’re attempting to define two functions of the same name.
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
        2. The module’s name will be Palindrome, so your code should be in a file named **Palindrome.hs**. Your Palindrome module will have a **function, also named isPalindrome**, which will be the function used by the Main module. See unit6/lesson34/3Palindrome.hs or  ![Alt text](unit6/lesson34/robustPalindrome.png?raw=true "Robust Palindrome") <p align="center"> Robust Palindrome </p>
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
    4. Different ways to write import module ways to write :
        1. where program is in the module file
           1.  `module Palindrome (isPalindrome, preprocess) where ` .
           2. method 2
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
    5. Note `import qualified Palindrome` vs `import Palindrome ()` in unit6/lesson34/Main.hs
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
          1. LICENSE
          2. src
          3. Setup.hs
          4. stack.yaml
          5. app
          6. test
          7. palindrone-checker.cabal
        8. Other templates are available in https://github.com/commercialhaskell/stack-templates is a source for many
        9. **cabal** palindrome-checker.**cabal**, your project configuration file contains all the metadata related to your project e.g project name, version, description, *libaries* files location, haskell language
           1.  stack separates the code for your libraries vs the code for running your program into separate directories.
           3.  2 impt lines - `hs-source-dirs` (in **both** library and executable sections) and `exposed-modules`.
           4.  `hs-source-dirs` value tells you which subdirectory of your project your library files live in. Default is `src`
           5.  `exposed-modules` value tells you which libraries you’re using. `Lib` module default location is in `src/Lib.hs`. To add more values to `exposed-modules`, do
                ```
                exposed-modules:    Lib,
                                    Palindrome,
                                    Utils
                ```
           6. Small project , all the library functions can go into `src/Lib.hs`.
           7. Like the library section, the executable section also tells which file your `main` is located with the main-is value.
           8. Code for running program (executable): `app` directory (specified by `hs-source-dirs`) [where the program’s `Main` module lives via a Main.hs file inside]
           9. Gist: stack also automatically creates three directories for you:
              1.  app
              2.  src
              3.  test
          8.  Difference
            |  Library module               |Executable module                   |
            | -------------                 | -------------                      |
            | src                           | app                                |
            | src/Lib.hs.                   | app/Main.hs                        |
            | Lib.hs has the functions      | min& import Lib module from Lib.hs |

        10. Main module (generated by stack)
            ```
            module Main where

            import Lib      --> from src/Lib.hs

            main :: IO ()
            main = someFunc  -->> comes from Lib module in src/Lib.hs
            ```
        11. Lib module (generated by stack)
            ```
            module Lib
                ( someFunc
                ) where

            someFunc :: IO ()
            someFunc = putStrLn "someFunc"
            ```
        12. Your `Main` module logic should be minimal, relying primarily on library functions defined in the `Lib` module.
    3.  Overwrite `Lib.hs` stack with functions bcos simple project. Default original:
        ```
        module Lib
            ( someFunc
            ) where

        someFunc :: IO ()
        someFunc = putStrLn "someFunc"
        ```
    4. Overwrite `Main.hs` which is essential to building your executable, it goes in the app directory (this is declared in your project’s .cabal file!). Rem `import Lib` **This time you won’t use a qualified import.** Default original:
        ```
        module Main where
        import Lib -- ** impt
        main :: IO ()
        main = someFunc
        ```
    5A. Add edit your **.cabal** file to tell stack about any modules you’re depending on e.g. `Data.Text` into the `build-depends:` ![Alt text](unit6/lesson35/cabalEdits.png?raw=true "Add packages to dependencies") <p align="center"> Additions to build dependencies </p>
    6A. With everything together to build project, ensures that stack is using the correct version of GHC, run setup **in project directory** with:
        ```
        stack setup
        ```
    7A. Ensuring that your project is built with the version of GHC you used to write it is important. Specifying the version of GHC you want to use is done indirectly by choosing your stack resolver version. The stack resolver is set in the **stack.yaml** file: `resolver: lts-7.9`. The lts-7.9 version of the stack resolver uses GHC version 8.0.1. By default, stack uses the most recent stable resolver. Listing of the current resolver versions at www.stackage.org. For info on specific resolver, e.g 7.9 by www.stackage.org/lts-7.9 for the lts-7.9 resolver)
    8A. Build your project with `stack build`.
    9A. Run the project  with `exec` as in `stack exec palindrone-checker-exe` using the default 's `<project-name>-exe` The name of the project is taken from .cabal file line #38 `executable palindrone-checker-exe`
    10A. **QuickFix** For large program with `Data.Text`, add OverloadedStrings pragma to every file. Shortcut:
        1.  Go to .cabal file and universally apply the `OverloadString` language extension at
        2.  after default-language:  Haskell2010 to **both** your library and executable sections of .cabal:
            ```
            extensions: OverloadedStrings
            ```
    5B. **OTHERWISE** if the [.cabal#line3](https://github.com/1regina/GetProgrammingWithHaskell/blob/master/unit6/lesson35/palindrome-checker/palindrome-checker.cabal#L3) is `-- This file has been generated from package.yaml by hpack version 0.34.4.`, then go straight to **package.yaml**#dependencies and add instead . e.g to add text
        ```
        dependencies:
        - base >= 4.7 && < 5
        - text
        ```
    6B. then do `stack run`

