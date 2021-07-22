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
    4. functor explanation and type signature : ![Alt text](unit5/lesson27/fmapTypeSignature.png?raw=true "Fmap type signature") <p align="center"> Fmap type signature </p>
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
        5. `locationDB` will give you Maybe values but `haversine :: LatLong  -> LatLong  -> Double` and should be ![Alt text](unit5/lesson28/2locationsToDist_Type_Signature.png?raw=true "Type Signature for Function to connect locationsDb to printDistance") <p align="center"> Type Signature for Function to connect locationsDb to printDistance </p>
           1. It is almost similar to `haversine`, but everything is in the context of a `Maybe`.
           2. with `Functor` : we use normal functions in a context.
           3. Problems with `haversineMaybe` are :
              1. wrapper for any similar function ---> repetitive
              2. a different `haversineMaybe` for other context e.g `IO`.
           4. Limitations of a Functor ![Alt text](unit5/lesson28/fmapTypeSignature.png?raw=true "Functor's only method - fmap") <p align="center"> Functor's only method - fmap </p> : `fmap` function takes any function from type a to type b (where type a COULD be type b also), and the value of type a in the context of a Functor (like Maybe), and returns a value of type b in the same context.
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
    4. Applicative contains a method that’s the `<*>` operator. `:t (<*>)` returns
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
    5.  can use <$> and <*> to chain together any number of arguments.  Quick check 28.3  in  unit5/lesson28/1distance.hs ![Alt text](unit5/lesson28/applicativeWithFmapCompute.png?raw=true "Combine applicative and fmap in a Maybe context for partial application") <p align="center"> Combine applicative and fmap in a Maybe context for partial application </p>
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
        1. This enable Functor extension to multi-argument functions.
        2. Often bcos partial application in Haskell programs, common to wind up with a function in a context. With `Applicative`, we can use these functions.

24. Ch 29.0:  Lists as context/ A deeper look at the Applicative Type class.
    1. Applicative type class allows you to use functions that are inside a context, such as Maybe or IO -- extending the power of `Functor` type class.
    2. Applicative works with
    3. Functor, Functor is a superclass of Applicative. ![Alt text](unit5/lesson29/functorVapplicative.png?raw=true "Applicative v Functor Type signature") <p align="center"> Applicative vs Functor Type signature </p>
    4. `<*>` has the same type signature as your fmap, except the function argument is also in a context. This small difference in `<*>` allows you to chain together larger sequences of functions inside members of the Functor type class. ![Alt text](unit5/lesson29/applicativeTypeClassDef.png?raw=true "Type Class definition of Applicative") <p align="center"> Type Class definition of Applicative </p>
    5. Remember, because of fmap and <*>, you don’t need to rewrite any functions to work with Maybe values.
    6. function `pure` is the second method required by the `Applicative` type class. The `pure` method is a useful helper function for taking an ordinary value or function and putting it into a context (e.g an `Int` into a `Maybe Int`), allowing all possible computations in a context.
       1. Put value in a context: For `Maybe`, `pure` gives a `Just`
            ```
                1. into Maybe context
                -- >>> pure 6 :: Maybe Int
                -- Just 6

                2. into IO context
                hello :: IO String
                hello = pure "Hello World"
                -- >>> hello
                -- "Hello World"
            ```
       2. Put function in a context: use `pure` to put a function into the context of `Applicative`. e.g add  6 to (Just 5), you can use either fmap or pure:
            ```
                Prelude> (6+) <$> Just 5
                Just 11
                Prelude> pure (6+) <*> Just 5
                Just 11
                Prelude> (+6) <$> Just 5
                Just 11
                Prelude> pure (+6) <*> Just 5
                Just 11
            ```
    7. Applicative and Monad are for types as a context
       1. Parameterized types that represent a **container are types that represent a data structure**. The best test of whether a type is a container is whether you can tell what it does, independent of its name.
       2. When a type is a context, extra information is implied about the type, beyond its structure, beyond its structure.
    8. Structure over name: Containver vs Context
       1. data Blah a b = Blah a b is like a regular tuple
       2. The Data.Map type is another, similar structure. You could call this a Dictionary, a Binary-SearchTree, a MagicLookupBox, and so forth. But what the type means is implied by the data structure itself. Contexts differ from containers in that they require you to understand something about the computation happening beyond what the data structure alone tells you.
       3. **The 2-tuple (,) and Data.Map are both instances of Functor but not instances of Applicative.** The 2 are structures /containers
       4. Remember that the key power of Applicative is that it lets you apply a function in a parameterized type (containers)
       5. **IO and Maybe are context type**
       6. List: both a **container** and a **context**.
       7. List is a member of Applicative, so there must be a way to view List as a context. `:i Applicative` ---> `instance Applicative []`.
       8. Context question for a list:  context matters for a list is that to use Applicative, you need to be able to answer the question, “What does it mean to *apply a function to two or more values in the context of a list*?” For example, what does [1000,2000,3000] + [500,20000] mean? In terms of Applicative, you’d read this statement as follows: `pure (+) <*> [1000,2000,3000] <*> [500,20000]`. Adding together two Ints in the context of a List means adding all possible combinations of the values in those lists.
            ```
                Prelude> pure (+) <*> [1000,2000,3000] <*> [500,20000]
                [1500,21000,2500,22000,3500,23000]
            ```
       9. The best way to understand List as a context is that it describes nondeterministic computation. (computing multiple possibilities all at once.)
    9. List behaviour:
       1. A list as a container is a sequence of values that can hold any type. Each item in the list points to the next one or to the empty list.
       2. A list as a context represents a set of possibilities. Think of a list as a context as being a single variable that can contain many possible values. ![Alt text](unit5/lesson29/listAsContext.png?raw=true "List as a context (non-deterministic): all possibilities") <p align="center"> List as a context (non-deterministic computing): all possibilities </p>
    10.  Int context:
       1. **Maybe Int** is a single Int value in the context that it may be missing
       2. An **IO Int** is an Int value in the context that it was produced by an IOaction that may have produced side effects.
       3. An **[Int]** is an Int in the context that there are many possible values for that Int. With many possible values for that [Int], when you apply a function (Int -> Int -> Int) in the context of a list, think nondeterministically and compute all possible results of that operation. eg Game of 3 doors + 2 boxes gives 6 possibilties. The results of **adding two lists (list of 3 doors & list of 2 boxes) within the context** of a list are all **six** possible solutions in your non-deterministic world. ![Alt text](unit5/lesson29/pureApplicativeListContext.png?raw=true "pure applicative on list for nondeterministic") <p align="center"> pure applicative on list for non deterministic </p>
       4. To compute multiply of 2 lists
            ```
                doorPrize :: [Int]
                doorPrize = [1000,2000,3000]
                boxPrizeMultiplier :: [Int]
                boxPrizeMultiplier = [10,50]
                *Main> pure (*) <*>  doorPrize <*>
                boxPrizeMultiplier
                [10000,50000,20000,100000,30000,150000]
            ```
       5. Find prime numbers to N with composities via pure
            ```
            primesToN :: Integer -> [Integer]
            primesToN n = filter isNotComposite twoThroughN
                where twoThroughN = [2 .. n]
                    composite = pure (*) <*> twoThroughN <*> twoThroughN
                    isNotComposite = not . (`elem` composite)
            ```
       6. Generate/ Create data: Suppose you have a list of usernames, some typical and others problematic in certain cases. Thinking of lists as context,use the same pattern used for IO and Maybe to generate many test users. Use the **Data Constructor** as the **function** in `pure function <*> list1 <*> list2 <*> list3`
          1. For update: add whatever values you like to testNames, testIds, or testScores -> Quick  check  29.5 unit5/lesson29/listApplicative.hs    Add    your    own    name    to    testNames  and  regenerate  the  data.  How  many are there now?
            ```
            data User = User {
                    name :: String
                    , gamerId :: Int
                    , score :: Int
                    } deriving Show
            FOR changes in any of the lists, applicative with the amended list e.g testNamesAdd

            testNamesAdd :: [String]
            testNamesAdd = [ "Haskell Girl"
                            ,"John Smith"
                            ,"Robert'); DROP TABLE Students;--"
                            ,"Christina NULL"
                            ,"Randall Munroe"]

            testData1 :: [User]  -- using the Data Constructor as the function
            testData1 = pure User <*> testNamesAdd
                                  <*> testIds
                                  <*> testScores

            *Main> length testData1
            45
            ```
       7. generate test data that includes all possible combinations of certain list values means nondeterministically computing a list of possible users means use the Applicative properties `(pure function <*> list1 <*> list2 <*> list3)` of List to nondeterministically generate data.
       8. For use as replacement for `Functor` as `Applicative` is more powerful: `pure function <*> (a Maybe Value ~e.g from Map.lookup someKey`)
            ```
                pure lookupCredits <*> lookupUserName id
            ```

25. Ch30.0: Intro Monad Type Class
    1.  `fmap`: when you have a value in a context and a regular function, and want your result in a context.
        ```
        fmap :: (a -> b) -> f a -> f b
        GHCi> (+ 2) <$> Just 3
        Just 5
        ```
    2.  `Applicative`:
        1.  <*> allows you to connect a function in a context with values in a context,
            ```
            (<*>) :: f (a -> b) -> f a -> f b

            Applicative’s <*> allows you to apply a function in a context.

            maybeInc :: Maybe (Integer -> Integer)
            maybeInc = (+) <$> Just 1

            -- >>> maybeInc <*> Just 5 which is (+) <$> Just 1 <*> Just 5
            -- Just 6
            ```
        2. `pure` method allows you to handle the case of your final result not being in a context, so you can always put a result into a context.
            ```
            pure :: a -> f a
            hello :: IO String
            hello = pure "Hello World"
            ```
        3. `Applicative` `<*>` with `pure`
            ```
                GHCi> pure (+) <*> Just 3 <*> Just 2
                Just 5
            ```
    3. `Monad` bind **>>=** for when the initial argument isn’t in a context but its result is so it is for using any possible function in a context.
       1. Combine 2 Map lookups to look up a value in one Map in order to access another value in a second Map e.g employeeName to ID to bloodtype/ department.  **>>= allows you to chain together a sequence of function of a type `(a -> m b)`**.
          1. Maybe context Monad application
                ```
                creditsFromId :: GamerId -> Maybe PlayerCredits
                creditsFromId id = lookupUserName id >>= lookupCredits

                lookupUserName id = Map.lookup id userNameDB
                lookupCredits username = Map.lookup username creditsDB

                lookupGamerId :: WillCoId -> Maybe GamerId
                lookupGamerId id = Map.lookup id gamerIdDB

                -- Monad in action
                creditsFromWCId :: WillCoId -> Maybe PlayerCredits
                creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits
                ```
          2. IO context Monad application. See unit5/lesson30/2monadIO.hs
            ```
                helloName :: IO ()
                helloName = askForName >>
                            getLine >>= (\name ->return (nameStatement name))>>=
                            putStrLn
            ```
       2. type signature.
            ```
            (>>=) :: Monad m => m a -> (a -> m b) -> m b
            ```
       3. `>>=` is a member of the Monad type class. Maybe and IO are both instances of Monad, which means you can use >>= to solve Maybe and IO problems.
            ```
            Prelude> :i Monad
                class Applicative m => Monad (m :: * -> *) where
                (>>=) :: m a -> (a -> m b) -> m b
                (>>) :: m a -> m b -> m b
                return :: a -> m a....
                instance Monad Maybe
                instance Monad IO
            ```
    4. Use `Applicative` constraint rather than `Functor` only because `Applicative` is more powerful. If you can’t solve your problem with `Applicative`, you can’t solve it with `Functor` either.
        ```
        (<$>) :: Functor f => (a -> b) -> f a -> f b
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b
        pure :: Applicative f => a -> f a
        ```
    5. With **<$>, <*>, pure, and >>=**, you can chain together any computation you need in a context. ![Alt text](unit5/lesson30/functorApplicativeMonadTypeSignature.png?raw=true "Functor vs Applicative vs Monad Type Signatures") <p align="center"> Functor vs Applicative vs Monad Type Signatures </p>
    6. Monad has  four important methods in your type class definition.
        1.   only method required for the minimum definition of Monad is **>>=**.  >>= lets you chain together a function (of type `(a -> m b)`) that put a normal value into a context.
        2.   The **fail** method handles the case of errors happening in your Monad.
             1.   For Maybe, fail returns Nothing;
             2.   For IO, fail raises an I/O error.
        3. **return** method is similar to `pure`.  only difference is: `pure` has a type class restraint on *Applicative*, whereas `return` has a constraint on the *Monad* type class bcos  `Monad` type class predates the `Applicative` type class, so the `return` method exists for legacy reasons. In context of `Monad`, stick with `return`. ![Alt text](unit2/lesson14/TypeclassRoadMap.png?raw=true "Type Classes Map") <p align="center"> Type Classes Map </p>
            ```
            pure :: Applicative f => a -> f a
            return ::     Monad m => a -> m a
            ```
        4. last method **>>** operator throws away the first `m a` type e.g in `IO` context when using `putStrLn`, you don’t get anything back ie print something to user and throw away the IO ( ) `putStrLn` result

    7. `Monad` **>>** operator: When working with IO, >> is useful anytime you need to perform an IO action that doesn’t meaningfully return a value.
       1. send the results (hello + name) to putStrLn, and your action is finished. You start with chaining together askForName and getLine with >>, because you don’t need the results:(askForName >> getLine)
       2. have an IO String, but you need to connect it with name-Statement, which is a regular String -> String function. You can use `>>=` to do this if you can make nameStatement return an IO String. Solution is to wrap nameStatement in a lambda and use return at the end. ![Alt text](unit5/lesson30/usingMonadReturn.png?raw=true "using Monad's return method") <p align="center"> using Monad's return method </p>
    8. Last note: The Monad type class is the final refinement of computing in a context started with Functor. The most important method of the Monad type class is the >>= (pronounced bind) operator. You use >>= to chain together functions of the type (a -> m b) which is particularly important for working with the IO type. Unlike Maybe, you can’t trivially use pattern matching to access values inside the IO context. The Monad type class is what makes I/O programming possible.
    9. Q30.1 Universal fmap for applicative <*> and monad >>= instances
            ```
            allFmap :: Applicative f => (a -> b) -> f a -> f b
            allFmap function applicInstance = pure function <*> applicInstance

            -- recall also (getLine >>= (\name ->return (nameStatement name))) in unit5/lesson30/2monadIO.hs

            allFmapM :: Monad m => (a -> b) -> m a -> m b
            allFmapM function val = val >>= (\x -> return (function x))
            ```
    10. Q30.2 Monad is strictly more powerful than Applicative, write a universal version of `<*>`, called allApp, that defines `<*>` for all members of the Monad type clas
            ```
            allApp :: Monad m => m (a -> b) -> m a -> m b
            -- <*> :: Applicative f :: f(a -> b) -> f a -> f b
            -- >>= :: Monad  m :: m a -> (a -> m b) -> m b

            -- Two hints:
            -- 1. Try to think exclusively in terms of the type signatures.
            -- 2. Use <$> if you want and replace it with your answe    r to Q29.1
            -- recall creditsFromId id = lookupUserName id >>= lookupCredits

            allApp function val = function >>= (\f -> val  >>= (\x -> return (f x)))
            ```
    11. Q30.3 Implement a bind function which is the same as (>>=) for Maybe:
            ```
            bind :: Maybe a -> (a -> Maybe b) -> Maybe b
            bind Nothing _ = Nothing
            bind (Just val) function = function val
            ```
26. Ch31.0: Making Monads easier with do-notation
    1. Two useful tools that make working with Monads significantly easier.
       1. The first is `do-notation`
       2. The second list comprehension as `List` can work as a `Monad`
    2. Monad refresher:
       1. example
            ```
                askForName :: IO ()
                askForName = putStrLn "What is your name?"

                nameStatement :: String -> String
                nameStatement name = "Hello, " ++ name ++ "!"

                helloName :: IO ()
                helloName = askForName >>
                            getLine >>=
                            (\name ->
                                return (nameStatement name)) >>=
                            putStrLn
            ```
       2. >> allows you to perform an **IO action** and chain it with another action, ignoring its value.`(>>) :: Monad m :: m a -> m b --> m b`. ie `askForName >> getLine `
       3. >>= allows you to perform an IO action and then hand off the return value of that function to another waiting for a value `(>>=) :: Monad m :: m a -> (a -> m b) -> m b` ie
          1. getLine **>>=** (\name -> return (nameStatement name))
          2. (\name -> return (nameStatement name)) **>>=** putStrLn
       4. *return* `return :: Monad m :: a -> m a` ie (\x -> return (func x)) allows you to take an ordinary function and have it work in the context of IO
       5. Rewriting helloName using do-notation.
            ```
                helloNameDo :: IO ()
                helloNameDo = do
                    askForName
                    name <- getLine
                    putStrLn (nameStatement name)

            ```
        ![Alt text](unit5/lesson31/monad2do.png?raw=true "Monad-to-do transformation") <p align="center"> Monad-to-do transformation. name missing in putStrLn (nameStatement name) </p>
       6. Rewriting do to Monad (2 more examples)![Alt text](unit5/lesson31/desugar_do.png?raw=true "do-to- transformation") <p align="center"> Monad-to-do transformation. name missing in putStrLn (nameStatement name) </p>
            ```
                helloPerson :: String -> String
                helloPerson name = "Hello" ++ " " ++ name ++ "!"

                helloPersonDo :: IO ()
                helloPersonDo = do
                        name <- getLine
                        let statement = helloPerson name
                        putStrLn statement

                helloPersonMonad :: IO ()
                helloPersonMonad = getLine >>=
                                (\name ->
                                    (\statement -> putStrLn statement)
                                                            (helloPerson name))
                ==========================================
                echoMonad :: IO ()
                echoMonad = getLine >>= putStrLn

                echoDo :: IO ()
                echoDo = do
                    someString <- getLine
                    putStrLn someString
            ```
        7. Comparing the costs of two pizzas. Because do-notation works on all members of Monad, you were able to trivially translate this program to work with Maybe types when your values came from Data.Maps rather than IO.
        8. Different contexts using **do** in unit5/lesson31/2monad_do_3Contexts.hs.**PLEASE see the file**
           1. **IO Context** bcos of the Monad type class, you have an easy way to take a Candidate that wasn’t designed with I/O in mind and use that Candidate in the IO context
                ```
                data Candidate = Candidate...

                viable :: Candidate -> Bool

                readCandidateIO :: IO Candidate

                assessCandidateIO :: IO String
                assessCandidateIO = do
                    candidate <- readCandidateIO....
                        let passed = viable candidate  -- where all tests == True
                        let statement = if passed
                                        then "passed"
                                        else "failed"
                        return statement
                ```
           2. **Maybe Context** Maybe is also a Monad
                ```
                candidate1 :: Candidate..., candidate2 :: Candidate..., candidate3 :: Candidate

                    -- 5B.2  Create a candidate DB
                    candidateDB :: Map.Map Int Candidate
                    candidateDB = Map.fromList [(1,candidate1)
                                            ,(2,candidate2)
                                            ,(3,candidate3)

                    -- need assessCandidateIO cousin for Maybe assessCandidateMaybe
                    assessCandidateMaybe :: Int -> Maybe String
                    assessCandidateMaybe cID = do
                        candidate <- Map.lookup cID candidateDB
                        let passed = viable candidate  -- where all tests == True
                        let statement = if passed
                                        then "passed"
                                        else "failed"
                        return statement
                ```
           3. **List Contex** List is also a Monad. Working with lists by using the tools of the Monad type class, you can treat entire lists as single values
                ```
                -- 5C.1 Possible Candidates in a list context
                candidates :: [Candidate]
                candidates = [candidate1
                            ,candidate2
                            ,candidate3]

                -- 5C.2 Assessing a list of candidates using List as a MonadListing
                assessCandidateList :: [Candidate] -> [String]
                assessCandidateList candidates = do
                        candidate <- candidates
                        let passed = viable candidate  -- where all tests == True
                        let statement = if passed
                                        then "passed"
                                        else "failed"
                        return statement
                ```
           4. *Fun Fact* `assessCandidateIO`  and `assessCandidateMaybe` and `assessCandidateList` are almost identical.  because after you assign a variable with <- in do-notation, you get to pretend it’s an ordinary type that’s not in a particular context. The Monad type class and do-notation have abstracted away the context you’re working in. =>  can start thinking about all problems in a context in the same way

    3. In IO `return` vs `print`
       1. `print x = putStrLn (show x)`
       2. `return :: Monad m => a -> m a` See [stackoverflow](https://stackoverflow.com/questions/20690304/type-of-return-in-do-block#:~:text=when%20learning%20about%20monads%20it's%20helpful%20)
        ```
        {-# LANGUAGE ScopedTypeVariables #-}

            test1 :: IO String
            test1 = do
            a <- getLine             :: IO String
            putStrLn a               :: IO ()
            return a                 :: IO String
            ====================================
            test2 :: IO String
            test2 = do
            (a :: String) <- getLine  :: IO String
            (() :: ()) <- putStrLn a  :: IO ()
            return a                  :: IO String
            ====================================
            test3 :: IO String
            test3 = getLine >>=
                    (\a -> putStrLn a >>=
                    \() -> return a)
        ```
    4.  `do-notation` and the `Monad` type class allow you to solve problems while abstracting away the context:
        1.  With `IO` , you can write code for IO types and not worry about the mismatch between IOStrings and regular Strings.
        2.  With `Maybe`, you can write code for Maybe and forget about dealing with missing values.
        3.  With `list`, you can even write code for lists and pretend you have only a single value.
    5. The only limitation to using the same code in all three contexts is that the type signatures are too restrictive. Because *IO, Maybe, and List* are all **instances of Monad**, you can use a **type class constraint** in your definition of a universal `assessCandidate` function. The amazing thing here is you need to change only the type signature of your `assessCandidateList` function to do this. `Monad` type class allows you to write code for regular types and use them in increasingly powerful ways in a context such as `Maybe`, `IO`, or `List`. 5D A **universal** `assessCandidate` function with simply a common type class constraint.
        ```
        assessCandidate :: Monad m =>  m Candidate -> m String
        assessCandidate candidates = do
                candidate <- candidates
                let passed = viable candidate  -- where all tests == True
                let statement = if passed
                                    then "passed"
                                    else "failed"
                return statement

        -- 5D.1 Test in IO context by `ghci 2monad_do_3Contexts.hs`
        -- *Main> assessCandidate readCandidateIO
        -- Record candidate results
        -- Enter candidate ID:
        -- 8
        -- Enter code review grade:
        -- A
        -- Enter cultural fit grade:
        -- A
        -- Enter highest education level:
        -- MS
        -- "passed"

        --5D.2 Test in Maybe context
        -- >>> assessCandidate (Map.lookup 1 candidateDB)
        -- Just "failed"
        -- >>> assessCandidate (Map.lookup 2 candidateDB)
        -- Just "failed"
        -- >>> assessCandidate (Map.lookup 3 candidateDB)
        -- Just "passed"
        -- >>> assessCandidate candidates
        -- ["failed","failed","passed"]
        ```
    6. Monad-do transformation unit5/lesson31/l31exercises.hs. pizza comparison
        ```
        -- do way of compare
        main :: IO ()
        main = do
            putStrLn "What is the size of pizza 1"
            size1 <- getLine
            putStrLn "What is the cost of pizza 1"
            cost1 <- getLine
            putStrLn "What is the size of pizza 2"
            size2 <-  getLine
            putStrLn "What is the cost of pizza 2"
            cost2 <- getLine
            let pizza1 = (read size1, read cost1)
            let pizza2 = (read size2, read cost2)
            let betterPizza = comparePizzas pizza1 pizza2
            putStrLn (describePizza betterPizza)

        =================
        -- Monad alternative for do notation
        cheaperPizza :: IO ()
        cheaperPizza =  putStrLn "What is the size of pizza 1" >>
                        getLine >>=
                        (\size1 ->
                            putStrLn "What is the cost of pizza 1" >>
                            getLine >>=
                            (\cost1 ->
                                putStrLn "What is the size of pizza 2" >>
                                getLine >>=
                                (\size2 ->
                                    putStrLn "What is the cost of pizza 2" >>
                                    getLine >>=
                                    (\cost2 ->
                                        (\pizza1 ->
                                            (\pizza2 ->
                                                (\betterPizza ->
                                                    putStrLn (describePizza betterPizza)
                                                )   (comparePizzas pizza1 pizza2)
                                            )(read size2 , read cost2)
                                        )(read size1, read cost1)
                                    ))))

        =========================
        -- Maybe Context
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

        =========================
        -- List Context
        listMain :: [String]
        listMain = do
            size1 <- [10,12,17]
            cost1 <- [12.0,15.0,20.0]
            size2 <- [10,11,18]
            cost2 <- [13.0,14.0,21.0]
            let pizza1 = (size1,cost1)
            let pizza2 = (size2,cost2)
            let betterPizza = comparePizzas pizza1 pizza2
            return  (describePizza betterPizza)

        =========================
        -- Universal Monad
        monadMain :: Monad m => m Double -> m Double
                    -> m Double -> m Double -> m String
        monadMain s1 c1 s2 c2 = do
            size1 <- s1
            cost1 <- c1
            size2 <- s2
            cost2 <- c2
            let pizza1 = (size1,cost1)
            let pizza2 = (size2,cost2)
            let betterPizza = comparePizzas pizza1 pizza2
            return  (describePizza betterPizza)


        ```

