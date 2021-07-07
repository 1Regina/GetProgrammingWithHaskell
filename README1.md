17. Ch21.0
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