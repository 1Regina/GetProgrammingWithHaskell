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

    8. Semigroup, Monoid,  and  now  Functor! are from Maths abstract algebra and category theory for design patterns of functional programming.
    9. functor kinds: lesson 18 -- kinds are the types of types.
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
        4. Map is a useful type for this DB example because it naturally involves three instances of Functor: it’s made from a List, returns Maybe values, and is itself a Functor. Build a Map is with the fromList function.NB: Haskell Map (dictionary is a list of tuples instead of key:value. There is do colon **`:`**)
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
    14. The Functor type class allows you to apply an ordinary function to values inside a container (for example, List) or a context (for example, IO or Maybe).
    15. Functor’s fmap (=the <$> operator) can do apply the `Int -> Double` function to the `Maybe Int` value, *resulting* in a `Maybe Double` value. `Functor` allow you to **reuse a single function (e.g renderHtml) with any type belonging to the Functor type class**. [Int], Maybe Int, and IO Int can all use the same core functions. Recap all HTML trnsformation with renderHTML. Functor’s < $> provides a common interface to apply any function to a value in a contex.
        ```
        partHtml :: Maybe Html
        partHtml = renderHtml <$> partVal      -- fmap in a Maybe context
        allPartsHtml :: [Html]
        allPartsHtml = renderHtml <$> allParts -- fmap in a list container
        htmlPartsDB :: Map.Map Int Html
        htmlPartsDB = renderHtml <$> partsDB   -- fmap in a Map ie Dictionary container
        htmlSnippet :: IO Html
        htmlSnippet = renderHtml <$> leftArmIO -- fmap in a IO context
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
        -- 1. ghc --make l27_2exercises.hs OR ghc l27_2exercises.hs
        -- 2. ./l27_2exercises
        -- 3. follow IO command to input ID range (1,3)
        -- 4. <Ctrl d>
        -- 5. output is shown
        ```

23. Ch28: Applicative Type Class: Using Functions in a context
    1.  The `Applicative` type class extends the power of Functor by allowing you to use functions that are themselves in a context.
    2.  `Functor` can’t do passing two Maybe values to a function but `Applicative` can. `Applicative` can help create data in the context of either IO or Maybe, while allowing you to reuse the majority of your code.
    3.  Case when Functor cant handdle 2 `Maybes` - a simple command-line application that allows the user to enter cities by name and then returns the distance between them but fail when city is non-existent in db. First try: use the `Maybe` type and the `Functor` type class will fail. See unit5/lesson28/1distance.hs
        1. Process
           1. get two locations from your locationDB
           2. calculate their distance, and
           3. then pass that distance to `printDistance`
        2. Using a Map as your database of city coordinates
            ```
            type LatLong = (Double,Double)
            locationDB :: Map.Map String LatLong
            locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                                    ,("Innsmouth",(42.8250,-70.8150))...
            ```
        3. convert latitude and longitude to radians first (round globe) and do Haversine
            ```
            toRadians :: Double -> Double
            toRadians degrees = degrees * pi / 180

            latLongToRads :: LatLong -> (Double,Double)
            latLongToRads (lat,long) = (rlat,rlong)
                where rlat = toRadians lat
                    rlong = toRadians long

            haversine :: LatLong -> LatLong -> Double
            haversine coords1 coords2 = earthRadius * c
                where (rlat1,rlong1) = latLongToRads coords1
                    (rlat2,rlong2) = latLongToRads coords2
            ```
        4. Command line tool for user to enter in two city names, and you’ll return the distance.
            ```
            printDistance :: Maybe Double -> IO ()
            .....cases for  Nothing + Just distances
            ```
        5. `locationDB` will give you Maybe values but `haversine :: LatLong  -> LatLong  -> Double` and should be ![Alt text](unit5/lesson28/2locationsToDist_Type_Signature.png?raw=true "Type Signature for Function to connect locationsDb to printDistance")
           1. It is almost similar to `haversine`, but everything is in the context of a `Maybe`.
           2. with `Functor` : we use normal functions in a context.
           3. Problems with `haversineMaybe` are :
              1. wrapper for any similar function ---> repetitive
              2. a different `haversineMaybe` for other context e.g `IO`.
           4. Limitations of a Functor ![Alt text](unit5/lesson28/fmapTypeSignature.png?raw=true "Functor's only method - fmap") : `fmap` function takes any function from type a to type b (where type a COULD be type b also), and the value of type a in the context of a Functor (like Maybe), and returns a value of type b in the same context.
           5. Problem we have 2 arguments but Functor has only 1 method (fmap). fmap only takes one argument the `f a`
           6. Close look:
              1. need `haversine :: (LatLong -> LatLong -> Double)`
              2. to take 2 `Maybe` which are `Maybe LatLong -> Maybe LatLong`
              3. finally answer in a `Maybe: Maybe Double`
              4. snapshot view **INCORRECT**
                ```
                (LatLong -> LatLong -> Double) ->
                        (Maybe LatLong ->  Maybe LatLong -> Maybe Double)

                another view: Functor with 1 extra argument!! WRONG!
                Functor f => (a -> b -> c) -> f a -> f b -> f c

                ```
              5. **Functor’s fmap: it only works on single-argument functions.**
              6. 1 possible Solution: Partial Application - partial application means that calling an argument first so it results in a reduced function waiting only for the remaining (unaccounted) arguments. e.g distanceFromNY but in this case, this means
                 1. having to set up function for every other cities to calculate distance from them.
                 2. a function in a context like maybeInc :: Maybe (Integer -> Integer)  it’s inside a Maybe. You now have a Maybe function, but there’s no way to apply this function!
           7. **all functions are functions of one argument. Multi-argument functions are just a chain of single-argument functions.**
    4. Applicative contains a method that’s the <*> operator. :t (<*>) returns
        ```
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b

        example breakdown
        (++) <$> Just "cats" <*> Just " and dogs"
        (++) is the (a -> b)
        <$> is the instance applicative instance f
         Just "cats" is the fa

        Prelude> :i (<*>)
        class Functor f => Applicative (f :: * -> *) where
        ...
        (<*>) :: f (a -> b) -> f a -> f b
        ```
        ![Alt text](unit5/lesson28/applicativeTypeSignature.png?raw=true "Applicative type signature")
        1. Applicative’s <*> allows you to apply a function in a context.
            ```
            maybeInc :: Maybe (Integer -> Integer)
            maybeInc = (+) <$> Just 1

            -- >>> maybeInc <*> Just 5 which is (+) <$> Just 1 <*> Just 5
            -- Just 6
            -- >>> maybeInc <*> Nothing
            -- Nothing
            ```
        2. Applicative combine Strings in a Maybe context as well:
            ```
            >>> (++) <$> Just "cats" <*> Just " and dogs"
            Just "cats and dogs"
            >>> (++) <$> Nothing <*> Just " and dogs"
            Nothing
            >>> (++) <$> Just "cats" <*> Nothing
            Nothing
            ```
    5.  can use <$> and <*> to chain together any number of arguments.  Quick check 28.3  in  unit5/lesson28/1distance.hs ![Alt text](unit5/lesson28/applicativeWithFmapCompute.png?raw=true "Combine applicative and fmap in a Maybe context for partial application")
        ```
        val1 = Just 10
        val2 = Just 5
        chain2 = (div) <$> val1 <*> val2
        chain3 = (mod) <$> val1 <*> val2
        ```
    6. Distance between two cities program continue step 3 above. Core functionality without worry of exception
        ```
        main :: IO ()
        main = do
            putStrLn "Enter the starting city name:"
            startingInput <- getLine
            let startingCity = Map.lookup startingInput locationDB
            putStrLn "Enter the destination city name:"
            destInput <- getLine
            let destCity = Map.lookup destInput locationDB
            let distance = haversine <$> startingCity <*> destCity
            printDistance distance

        -- steps :
        -- 1. ghc 1distance.hs
        -- 2. ./1distance
        ```
    7. Functor and Applicative complement this safety by making it easy to mix regular functions such as haversine with `Maybe` types or `IO` types, without compromising that safety
    8.  power of partial application and <*>, you can chain together as many arguments as you’d like e.g unit5/lesson28/min3.hs minOfThree val1 val2 val3 = **min** val1 *(**min** val2 val3)*
    9. 3 arguments (min3.hs) chained applicative `minOfThree <$> Just 10 <*> Just 3 <*> Just 6` Note the position of <*> for applicative.
    10. ***Maybe context*** create data with <$> and <*>
        ```
        data User = User
        { name :: String
        , gamerId :: Int
        , score :: Int
        } deriving Show

        serverUsername :: Maybe String
        serverUsername = Just "Sue"
        serverGamerId :: Maybe Int
        serverGamerId =  Just 1337
        serverScore :: Maybe Int
        serverScore = Just 9001

        -- >>> User <$> serverUsername <*> serverGamerId <*> serverScore
        -- Just (User {name = "Sue", gamerId = 1337, score = 9001})
        ```
    11. `readInt` function from the preceding lesson to transform user input directly to an Int
    12. ***IO context*** create data with <$> and <*>
        ```
        readInt :: IO Int
        readInt = read <$> getLine

        main :: IO ()
        main = do
            putStrLn "Enter a username, gamerId and score"
            user <- User <$> getLine <*> readInt <*> readInt
            print user
        ```
    14. powerful thing here is define only a single type, User, that works with regular Strings and Ints. With Applicative type class, use the same code to create a user in different contexts.
    15. Summary :  Applicative’s <*> operator allows you to use functions that are themselves in a context. If you have a function that might not exist, `Maybe (Int -> Double)`, you can apply it to a value in the same context, `Maybe Int`, and get a result still in that context, `Maybe Double`.
        1.  This enable Functor extension to multi-argument functions.
        2.  Often bcos partial application in Haskell programs, common to wind up with a function in a context. With `Applicative`, we can use these functions.

24. Ch 29.0:  Lists as context/ A deeper look at the Applicative Type class.
    1.  Applicative type class allows you to use functions that are inside a context, such as Maybe or IO -- extending the power of `Functor` type class.
    2.  Applicative works with Functor, Functor is a superclass of Applicative. ![Alt text](unit5/lesson29/functorVapplicative.png?raw=true "Applicative v Functor Type signature") <p align="center"> Applicative vs Functor Type signature </p>