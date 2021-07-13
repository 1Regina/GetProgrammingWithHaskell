22. Ch27: Functor
    1.  Functor type class provides a generic interface for applying functions to values in a container or context. Recap previous lessons:
        1. parameterized types (types that take another type as an argument) ([( , )] in organInventory in Ch18.0 )
        2. types that represent containers, such as `List` and `Map`
        3. parameterized types that represent a context, such as Maybe for missing values
        4. `IO` for values from the world of I/O.
    2. four different types, but they’re all parameterized by the same type: `Int`
       1. [Int]
       2. Map String Int
       3. Maybe Int
       4. IO Int
    3. can do computation on a potentially missing value without having to worry about whether it’s actually missing.
       1. Need to handle case of null values by not writing the value ie to keep your value in a Maybe: (in unit5/lesson27/functor.hs)
            ```
            successfulRequest :: Maybe Int
            successfulRequest = Just 6

            failedRequest :: Maybe Int
            failedRequest = Nothing

            -- when Null , value wont b written. Increase +1 only when it is an int
            incMaybe :: Maybe Int -> Maybe Int
            incMaybe (Just n) = Just (n + 1)
            incMaybe Nothing = Nothing

            -- quickcheck 27.1 a reverse fn for Maybe String
            reverseMaybe :: Maybe String -> Maybe String
            reverseMaybe (Just stringy) = Just (reverse stringy)
            reverseMaybe Nothing = Nothing
            ```
       2. Above scaling is difficult bcos needs a version for every function that use a `Maybe`. Solution **functor**
    4. functor explanation and type signature : ![Alt text](unit5/lesson27/fmapTypeSignature.png?raw=true "Fmap type signature")
       1. <$>, which is a synonym for fmap (except it’s a binary operator rather than a function).
    6. Making Maybe an instance of Functor:
        ```
        instance Functor Maybe where
            fmap func (Just n) = Just (func n)
            fmap func Nothing = Nothing
        ```
    7. `fmap` is the official function name, in practice the binary operator <$> is used much more frequently:
        ```
        -- >>>  fmap (+ 1) successfulRequest
        -- Just 7
        -- >>> (+1) <$>  successfulRequest
        -- Just 7
        -- >>> fmap (+ 1) failedRequest
        -- Nothing
        -- >>> (+1) <$> failedRequest
        -- Nothing
        ```
    8. Note type signature of the function in fmap is (a -> b), meaning that the Maybe returned doesn’t need to be parameterized by the same type.
       1. Two examples from a Maybe Int to a Maybe String..recall `successfulRequest :: Maybe Int` & `failedRequest :: Maybe Int`
        ```
         successStr :: Maybe String
         successStr = show <$> successfulRequest
         -- >>> successStr
         -- Just "6"
         failStr :: Maybe String
         failStr = show <$> failedRequest
         -- >>> failStr
         -- Nothing

         -- Quick check 27.2 Use fmap or <$> to reverse a MaybeString
        -- >>> fmap reverse (Just "sing to me")
        -- Just "em ot gnis"
        -- >>> reverse <$> (Just "sing to me")
        -- Just "em ot gnis"
        ```
       3. ability to transform the types of values inside a Maybe is the *true power of the Functor type* class

    9.  Semigroup, Monoid,  and  now  Functor! are from Maths abstract algebra and category theory for design patterns of functional programming.
    10. functor kinds: lesson 18 -- kinds are the types of types.
        1.  Types of a kind * -> * are parameterized types that take just one type parameter.
        2.  All Functors must be of kind * -> *. It also turns out that
        3.  many parameterized types of kind * -> * are instances of Functor
        4.  **Members of Functor**: `List`, `Map`, `Maybe`, and `IO`.
    11. Example of **Members of Functor**: `List`, `Map`, `Maybe`, and `IO` with functor (code in unit5/lesson27/robotPartsDBfmapListMapMaybeIO.hs) for RobotPart database
        1. At top for implementation of maps from keys to values (dictionaries)
            ```
            import qualified Data.Map as Map
            ```
        2. RobotPart defined using **record syntax**
            ```
            data RobotPart = RobotPart
                { name :: String
                , description :: String
                , cost :: Double
                , count :: Int
                } deriving Show
            -- Example robot parts: leftArm, rightArm, and robotHead
            leftArm :: RobotPart
            leftArm  = RobotPart
                { name = "left arm"
                , description = "left arm for face punching!"
                , cost = 1000.00
                , count = 3
                }
            rightArm :: RobotPart
            rightArm  = RobotPart
                { name = "right arm"
                , description = "right arm for kind hand gestures"
                , cost = 1025.00
                , count = 5
                }
            robotHead :: RobotPart
            robotHead  = RobotPart
                { name = "robot head"
                , description = "this head looks mad"
                , cost = 5092.25
                , count = 2
                }
            ```
        3. render the information contained in a RobotPart as HTML. code for rendering an individual RobotPart as an HTML snippet:
            ```
            type Html = String
            renderHtml :: RobotPart -> Html
            renderHtml part = mconcat ["<h2>",partName, "</h2>"
                                    ,"<p><h3>desc</h3>",partDesc
                                    ,"</p><p><h3>cost</h3>"
                                    ,partCost
                                    ,"</p><p><h3>count</h3>"
                                    ,partCount,"</p>"]
                where partName = name part
                    partDesc = description part
                    partCost = show (cost part)
                    partCount = show (count part)
            ```
        4. Map is a useful type for this DB example because it naturally involves three instances of Functor: it’s made from a List, returns Maybe values, and is itself a Functor. Build a Map is with the fromList function.
            ```
            partsDB :: Map.Map Int RobotPart
            partsDB = Map.fromList keyVals
                where keys = [1,2,3]
                    vals = [leftArm,rightArm,robotHead]
                    keyVals = zip keys vals
            ```
        5. Converting a Maybe RobotPart to Maybe Html. For a website driven by partsDB, should have request containing an ID for a part.
           1. `insertSnippet IO` action will take HTML and insert it into a page’s template
           2. many data models generating snippets and one of these models may have an error so `insertSnippet` accepts `Maybe` Html as its input, allowing the template engine to handle miss-ing snippets
            ```
                insertSnippet :: Maybe Html -> IO ()
            ```
        6. look up a part and pass it as Maybe Html to `insertSnippet`. **Map.lookup** to pick 1 value in a Data.Map ie dictionary.
            ```
            partVal :: Maybe RobotPart
            partVal = Map.lookup 1 partsDB
            ```
        7.  [*fmap with a **Maybe***]: <$> to transform RobotPart to HTML - Because Maybe is a Functor, can use <$> to transform RobotPart into HTML while remaining in a Maybe
            ```
            partHtml :: Maybe Html
            partHtml = renderHtml <$> partVal
            ```
        8. Next stage is Converting a list of RobotParts to a list of HTML. First, create an index page of all the parts by get a list of parts from your partsDB. **Map.toList** to put the extracted values into a list.
            ```
            allParts :: [RobotPart]
            allParts = map snd (Map.toList partsDB)

            -- fmap for list = map
            allParts1 :: [RobotPart]
            allParts1 = snd <$> (Map.toList partsDB)
            ```
        9. [*fmap with a **list***]: **List is also an instance of Functor.** `fmap` for a List = `map`. Transform a list of RobotParts to HTML with <$> instead of map
            ```
            allPartsHtml :: [Html]
            allPartsHtml = renderHtml <$> allParts

            -- Equivalent alternative as <$> is just fmap, and for lists fmap is just map
            allPartsHtml :: [Html]
            allPartsHtml = map renderHtml allParts
            ```
        10. [*fmap with a **Map ie Dictionary***] just have an htmlPartsDB for converting RobotParts to HTML to avoid continual conversion. Turn partsDB into a Map of HTML rather than RobotParts with fmap ie <$>
            ```
            htmlPartsDB :: Map.Map Int Html
            htmlPartsDB = renderHtml <$> partsDB

            -- >>> htmlPartsDB
            -- fromList [(1,"<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.....
            -- >>> Map.lookup 1 htmlPartsDB
            -- Just "<h2>left arm</h2><p><h3>desc......
            ```
        11. [*fmap with a **IO***]
            1.  using return to create an IO type of a RobotPart
            2.  turn this into HTML so that you can write the HTML snippet to a file/something
            ```
            leftArmIO :: IO RobotPart
            leftArmIO = return leftArm

            htmlSnippet :: IO Html
            htmlSnippet = renderHtml <$> leftArmIO
            ```
    12. One way to think of the Functor type class is as “things that can be mapped over. **As <$> is just fmap, and for lists, fmap is just map**.
    13. KINDS: Map’s kind is * -> * -> * because Map takes two type arguments 1 Key + 1 Value. Functors must be of kind * -> *. `<$> on your partsDB` makes it clear. Functor for Map is concerned only about the Map’s values and not its keys. When Map is made an instance of Functor, only concerned about a single type variable, the one used for its values ie as being of kind * -> *.
    14. **Functor’s <$> provides a common interface to apply any function to a value in a context**. For types such as `List` and `Map`, this is a convenient way to update values in these *containers*. For `IO`, it’s essential to be able to change values in an *IO context*, *because you can’t take IO values out of their context*.
    15. The Functortype class allows you to apply an ordinary function to values inside a container (for example, List) or a context (for example, IO or Maybe).
    16.  Functor’s fmap (=the <$> operator) can do apply the `Int -> Double` function to the `Maybe Int` value, *resulting* in a `Maybe Double` value. `Functor` allow you to **reuse a single function (e.g renderHtml) with any type belonging to the Functor type class**. [Int], Maybe Int, and IO Int can all use the same core functions. Recap all HTML trnsformation with renderHTML. Functor’s <$> provides a common interface to apply any function to a value in a contex.
    ```
    -- partHtml :: Maybe Html
    -- partHtml :: Maybe Html
23. Ch27: Functor
    1.  Functor type class provides a generic interface for applying functions to values in a container or context. Recap previous lessons:
        1. parameterized types (types that take another type as an argument) ([( , )] in organInventory in Ch18.0 )
        2. types that represent containers, such as `List` and `Map`
        3. parameterized types that represent a context, such as Maybe for missing values
        4. `IO` for values from the world of I/O.
    2. four different types, but they’re all parameterized by the same type: `Int`
       1. [Int]
       2. Map String Int
       3. Maybe Int
       4. IO Int
    3. can do computation on a potentially missing value without having to worry about whether it’s actually missing.
       1. Need to handle case of null values by not writing the value ie to keep your value in a Maybe: (in unit5/lesson27/functor.hs)
            ```
            successfulRequest :: Maybe Int
            successfulRequest = Just 6

            failedRequest :: Maybe Int
            failedRequest = Nothing

            -- when Null , value wont b written. Increase +1 only when it is an int
            incMaybe :: Maybe Int -> Maybe Int
            incMaybe (Just n) = Just (n + 1)
            incMaybe Nothing = Nothing

            -- quickcheck 27.1 a reverse fn for Maybe String
            reverseMaybe :: Maybe String -> Maybe String
            reverseMaybe (Just stringy) = Just (reverse stringy)
            reverseMaybe Nothing = Nothing
            ```
       2. Above scaling is difficult bcos needs a version for every function that use a `Maybe`. Solution **functor**
    4. functor explanation and type signature : ![Alt text](unit5/lesson27/fmapTypeSignature.png?raw=true "Fmap type signature")
       1. <$>, which is a synonym for fmap (except it’s a binary operator rather than a function).
    5. Making Maybe an instance of Functor:
        ```
        instance Functor Maybe where
            fmap func (Just n) = Just (func n)
            fmap func Nothing = Nothing
        ```
    6. `fmap` is the official function name, in practice the binary operator <$> is used much more frequently:
        ```
        -- >>>  fmap (+ 1) successfulRequest
        -- Just 7
        -- >>> (+1) <$>  successfulRequest
        -- Just 7
        -- >>> fmap (+ 1) failedRequest
        -- Nothing
        -- >>> (+1) <$> failedRequest
        -- Nothing
        ```
    7. Note type signature of the function in fmap is (a -> b), meaning that the Maybe returned doesn’t need to be parameterized by the same type.
       1. Two examples from a Maybe Int to a Maybe String..recall `successfulRequest :: Maybe Int` & `failedRequest :: Maybe Int`
        ```
         successStr :: Maybe String
         successStr = show <$> successfulRequest
         -- >>> successStr
         -- Just "6"
         failStr :: Maybe String
         failStr = show <$> failedRequest
         -- >>> failStr
         -- Nothing

         -- Quick check 27.2 Use fmap or <$> to reverse a MaybeString
        -- >>> fmap reverse (Just "sing to me")
        -- Just "em ot gnis"
        -- >>> reverse <$> (Just "sing to me")
        -- Just "em ot gnis"
        ```
       2. ability to transform the types of values inside a Maybe is the *true power of the Functor type* class

    8.  Semigroup, Monoid,  and  now  Functor! are from Maths abstract algebra and category theory for design patterns of functional programming.
    9.  functor kinds: lesson 18 -- kinds are the types of types.
        1.  Types of a kind * -> * are parameterized types that take just one type parameter.
        2.  All Functors must be of kind * -> *. It also turns out that
        3.  many parameterized types of kind * -> * are instances of Functor
        4.  **Members of Functor**: `List`, `Map`, `Maybe`, and `IO`.
    10. Example of **Members of Functor**: `List`, `Map`, `Maybe`, and `IO` with functor (code in unit5/lesson27/robotPartsDBfmapListMapMaybeIO.hs) for RobotPart database
        1. At top for implementation of maps from keys to values (dictionaries)
            ```
            import qualified Data.Map as Map
            ```
        2. RobotPart defined using **record syntax**
            ```
            data RobotPart = RobotPart
                { name :: String
                , description :: String
                , cost :: Double
                , count :: Int
                } deriving Show
            -- Example robot parts: leftArm, rightArm, and robotHead
            leftArm :: RobotPart
            leftArm  = RobotPart
                { name = "left arm"
                , description = "left arm for face punching!"
                , cost = 1000.00
                , count = 3
                }
            rightArm :: RobotPart
            rightArm  = RobotPart
                { name = "right arm"
                , description = "right arm for kind hand gestures"
                , cost = 1025.00
                , count = 5
                }
            robotHead :: RobotPart
            robotHead  = RobotPart
                { name = "robot head"
                , description = "this head looks mad"
                , cost = 5092.25
                , count = 2
                }
            ```
        3. render the information contained in a RobotPart as HTML. code for rendering an individual RobotPart as an HTML snippet:
            ```
            type Html = String
            renderHtml :: RobotPart -> Html
            renderHtml part = mconcat ["<h2>",partName, "</h2>"
                                    ,"<p><h3>desc</h3>",partDesc
                                    ,"</p><p><h3>cost</h3>"
                                    ,partCost
                                    ,"</p><p><h3>count</h3>"
                                    ,partCount,"</p>"]
                where partName = name part
                    partDesc = description part
                    partCost = show (cost part)
                    partCount = show (count part)
            ```
        4. Map is a useful type for this DB example because it naturally involves three instances of Functor: it’s made from a List, returns Maybe values, and is itself a Functor. Build a Map is with the fromList function.
            ```
            partsDB :: Map.Map Int RobotPart
            partsDB = Map.fromList keyVals
                where keys = [1,2,3]
                    vals = [leftArm,rightArm,robotHead]
                    keyVals = zip keys vals
            ```
        5. Converting a Maybe RobotPart to Maybe Html. For a website driven by partsDB, should have request containing an ID for a part.
           1. `insertSnippet IO` action will take HTML and insert it into a page’s template
           2. many data models generating snippets and one of these models may have an error so `insertSnippet` accepts `Maybe` Html as its input, allowing the template engine to handle miss-ing snippets
            ```
                insertSnippet :: Maybe Html -> IO ()
            ```
        6. look up a part and pass it as Maybe Html to `insertSnippet`. **Map.lookup** to pick 1 value in a Data.Map ie dictionary.
            ```
            partVal :: Maybe RobotPart
            partVal = Map.lookup 1 partsDB
            ```
        7.  [*fmap with a **Maybe***]: <$> to transform RobotPart to HTML - Because Maybe is a Functor, can use <$> to transform RobotPart into HTML while remaining in a Maybe
            ```
            partHtml :: Maybe Html
            partHtml = renderHtml <$> partVal
            ```
        8. Next stage is Converting a list of RobotParts to a list of HTML. First, create an index page of all the parts by get a list of parts from your partsDB. **Map.toList** to put the extracted values into a list.
            ```
            allParts :: [RobotPart]
            allParts = map snd (Map.toList partsDB)

            -- fmap for list = map
            allParts1 :: [RobotPart]
            allParts1 = snd <$> (Map.toList partsDB)
            ```
        9. [*fmap with a **list***]: **List is also an instance of Functor.** `fmap` for a List = `map`. Transform a list of RobotParts to HTML with <$> instead of map
            ```
            allPartsHtml :: [Html]
            allPartsHtml = renderHtml <$> allParts

            -- Equivalent alternative as <$> is just fmap, and for lists fmap is just map
            allPartsHtml :: [Html]
            allPartsHtml = map renderHtml allParts
            ```
        10. [*fmap with a **Map ie Dictionary***] just have an htmlPartsDB for converting RobotParts to HTML to avoid continual conversion. Turn partsDB into a Map of HTML rather than RobotParts with fmap ie <$>
            ```
            htmlPartsDB :: Map.Map Int Html
            htmlPartsDB = renderHtml <$> partsDB

            -- >>> htmlPartsDB
            -- fromList [(1,"<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.....
            -- >>> Map.lookup 1 htmlPartsDB
            -- Just "<h2>left arm</h2><p><h3>desc......
            ```
        11. [*fmap with a **IO***]
            1.  using return to create an IO type of a RobotPart
            2.  turn this into HTML so that you can write the HTML snippet to a file/something
            ```
            leftArmIO :: IO RobotPart
            leftArmIO = return leftArm

            htmlSnippet :: IO Html
            htmlSnippet = renderHtml <$> leftArmIO
            ```
    11. One way to think of the Functor type class is as “things that can be mapped over. **As <$> is just fmap, and for lists, fmap is just map**.
    12. KINDS: Map’s kind is * -> * -> * because Map takes two type arguments 1 Key + 1 Value. Functors must be of kind * -> *. `<$> on your partsDB` makes it clear. Functor for Map is concerned only about the Map’s values and not its keys. When Map is made an instance of Functor, only concerned about a single type variable, the one used for its values ie as being of kind * -> *.
    13. **Functor’s <$> provides a common interface to apply any function to a value in a context**. For types such as `List` and `Map`, this is a convenient way to update values in these *containers*. For `IO`, it’s essential to be able to change values in an *IO context*, *because you can’t take IO values out of their context*.
    14. The Functortype class allows you to apply an ordinary function to values inside a container (for example, List) or a context (for example, IO or Maybe).
    15.  Functor’s fmap (=the <$> operator) can do apply the `Int -> Double` function to the `Maybe Int` value, *resulting* in a `Maybe Double` value. `Functor` allow you to **reuse a single function (e.g renderHtml) with any type belonging to the Functor type class**. [Int], Maybe Int, and IO Int can all use the same core functions. Recap all HTML trnsformation with renderHTML. Functor’s <$> provides a common interface to apply any function to a value in a contex.
        ```
        partHtml :: Maybe Html
        partHtml = renderHtml <$> partVal      --[*fmap with a **Maybe***]
        allPartsHtml :: [Html]
        allPartsHtml = renderHtml <$> allParts -- [*fmap with a **list***]
        htmlPartsDB :: Map.Map Int Html
        htmlPartsDB = renderHtml <$> partsDB   -- [*fmap with a **Map ie Dictionary***]
        htmlSnippet :: IO Html
        htmlSnippet = renderHtml <$> leftArmIO -- [*fmap with a **IO***]
        ```
    16. Q27.3 Write a command-line interface for partsDB that lets the user look up the cost of an item, given an ID. Use the Maybe type to handle the case of the user entering missing input. Solution: reuse `data RobotPart` to `partsDB` from unit5/lesson27/2robotPartsDBfmapListMapMaybeIO.hs then add imports, showCost and main. Complete codes in unit5/lesson27/l27_2exercises.hs
        ```
        {-# LANGUAGE OverloadedStrings #-}
        import System.IO
        import System.Environment
        import qualified Data.Map as Map -- 1. for implementation of Map.fromList

        showCost :: Maybe Double -> IO()
        showCost Nothing = putStrLn  "No such item. Try again"
        showCost (Just cost) = print cost
        -- >>> cost robotHead
        -- 5092.25

        main :: IO ()
        main = do
            putStrLn "enter ID of item"
            itemID <- getContents
            let part = Map.lookup (read itemID) partsDB --find the item
            showCost (fmap cost part)    -- extract info wrt the item

        -- Steps to run
        -- 1. ghc --make l27_2exercises.hs
        -- 2. ./l27_2exercises
        -- 3. follow IO command to input ID range (1,3)
        -- 4. <Ctrl d>
        -- 5. output is shown
        ```

