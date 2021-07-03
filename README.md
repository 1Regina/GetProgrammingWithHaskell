Unit 1
1. Ch02: 3 rules on haskell functions
   1. All functions must take an argument
   2. All functions must return a value
   3. Anytime a function is called with the same argument, it must return the same value. Thus python += cannot exist in Haskell
2. Ch03: lambda is graceful
   1. with `where` at end:
      ```
      sumSquareOrSquareSum :: (Num p, Ord p) => p -> p -> p
                         sumSquareOrSquareSum x y = if sumSquare > squareSum
                                                      then sumSquare
                                                      else squareSum
                                       where sumSquare = x^2 + y^2
                                             squareSum = (x+y)^2
   2. with `let` :
      ```
      sumSquareOrSquareSum5 :: (Ord p, Num p) => p -> p -> p
                      sumSquareOrSquareSum5 x y = let sumSquare = (x^2 + y^2)
                                                      squareSum = ((x+y)^2)
                                                   in
                                                      if sumSquare > squareSum
                                                         then sumSquare
                                                      else squareSum
      ```
   3. lexical scope: nearest is kin:
      1. add3 is always 8 regardless of y:
      ```
      add3 y = (\y ->
                       (\x -> y + x) 1 ) 3
      ```
      2. doubleCube1 is always 64 : `doubleCube1 x = (\cube -> cube ** 3) (4)`
      3. BUT doubleCube will change according to different x: `doubleCube x = (\cube -> cube ** 3) (x * 2)`
3. Ch04: First class functions are functions that can
   1. take functions as input arguments or
   2. return function as its value output.
4. Ch05:
   1. When you capture a value inside a lambda function, this is referred to as a closure.
   2. Anytime you might want to use a closure (which in Haskell is pretty much anytime), you want to order your arguments from most to least general. e.g. `getRequestURL host apiKey resource id ` GHCi> getRequestURL "http://example.com" "1337hAsk3ll" "book" "1234" outputs "http://example.com/book/1234?token=1337hAsk3ll" .  Closures combine lambda functions and first-class functions to give you amazing power but partial application makes life even easier.
   3. `add4 a b c d = a + b + c + d`;  `addXto3 x = (\b c d ->add4 x b c d)` makes `addXto3` a closure awaiting 3 remaining arguments.
   4. Partial application: When you call any function with fewer than the required number of parameters in Haskell, you get a new function that’s waiting for the remaining parameters. This language feature is called **partial application**. eg `mystery = add4 3` so 3 becomes the standard staple that will always be included such that `mystery 2 3 4 = 12`
   5. In case where most to least general arguments order was not followed and need correction, instead of rewriting, try a way to **flip** them round. Given previously which shd have location first then name:
      ```
      addressLetter name location = locationFunction name
            where locationFunction = getLocationFunction location
      ```
      > Rectify by
      ```
      addressLetterV2 location name = addressLetter name location
      ```
      > To avoid repeating for each function, further rectify easily using lambda function, first-class functions, and a closure by
      ```
      flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)
      ```
      > This results in
      ```
      addressLetterV2 = flipBinaryArgs addressLetter
      addressLetterNY = addressLetterV2 "ny"
      ```
      > Proxy for partial application: function binaryPartialApplication that takes a binary function and one argument and returns a new function waiting for the missing argument. Binary function is one that needs 2 argument e.g (+, /, -, *)

      ```
      binaryPartialApplication binaryFunction arg = (\y -> binaryFunction arg y)
      ```
    6. Infix operators such as (+, /, -, *) can be used as a prefix function by putting parentheses around it:
         ```
         GHCi> 10 / 2
         5.0
         GHCi> (/) 10 2
         5.0
         ```
5. Ch06:
   1. The tail of a list with just one element is [].
   2. head and tail
      ```
      GHCi> head [[1,2],[3,4],[5,6]]
      [1,2]
      GHCi> tail [1,2,3]
      [2,3]
      GHCi> tail [3]
      []
      ```
   3. Infix operator (:), which is called cons. This term is short for construct. To make a list, you need to take a value and cons it with another list or empty list.
      ```
      GHCi> (1,2):(3,4):(5,6):[]
      [(1,2),(3,4),(5,6)]
      GHCi> 1:[2,3,4]
      [1,2,3,4]

      GHCi>['h','e','l','l','o']
      "hello"
      GHCi> 'h':'e':'l':'l':'o':[]
      "hello"
      ```
      > **every element of the list must be the same type.** e.g 1 char to a list of char
      ```
      "ello" is actually Prelude> 'e':'l':'l':'o':[] so 'h':"ello" is really just 'h':'e':'l':'l':'o':[]
      GHCi> 'h':"ello"
      "hello"
      ```
      > This fails because "h" (double quote) is a **list** of one character and the values inside "ello" are individual characters.
      ```
      Prelude> "h" : "ello"
      is equivalent to 'h' : [] : 'e':'l':'l':'o':[]
      <interactive>:7:7: error:
      • Couldn't match type ‘Char’ with ‘[Char]’
         Expected type: [[Char]]
         Actual type: [Char]
      • In the second argument of ‘(:)’, namely ‘"ello"’
         In the expression: "h" : "ello"
         In an equation for ‘it’: it = "h" : "ello"

      GHCi> "h":"ello"
      GHCi> ['h']:['e','l','l','o']
      GHCi>  'h':[]:'e':'l':'l':'o':[]  -- it's impossible
      ```
      > to join two lists , use ++
      ```
      GHCi> "h" ++ "ello"
      "hello"
      GHCi> [1] ++ [2,3,4]
      [1,2,3,4]
      ```
   4. The `!!` operator to access element of list by index
      > Infix
      ```
      GHCi> [1,2,3] !! 0
      1
      GHCi> "puppies" !! 4
      'i'
      ```
      > Prefix style
      ```
      GHCi> (!!) [1,2,3] 0
      1
      ```
      > Prefix with partial application
      ```
      GHCi> paExample1 = (!!) "dog"
      GHCi> paExample1 2
      'g'
      GHCi> paExample2 = ("dog" !!)
      GHCi> paExample2 2
      'g'
      ```
   5. Section = partial application on a binary operator. **Section needs to be wrapped in parentheses** like paExample2 or paExample3.
      ```
      GHCi> paExample3 = (!! 2)
      GHCi> paExample3 "dog"
      'g'
      ```
   6. *length* and *reverse* can be applied to list [] or strings "xad" .
      > isPalindrome word = word == reverse word
   7. `elem` to check a if a value is in a list
      ```
      GHCi> elem 13 [0,13 .. 100]
      True
      GHCi> elem 'p' "cheese"  --- note the 'p' is character and "cheese" is a list of character
      False
      ```
   8. Making a binary function to infix with ` `` ` .
      ```
      respond phrase = if '!' `elem` phrase
                      then "wow!"
                      else "uh.. okay"
      GHCi> respond "hello"
      "uh.. okay"
      GHCi> respond "hello!"
      "wow!"
      ```
   9. `take` and `drop`. `take` takes first n elements from a list. `drop` removes first n elements from a list. `drrop` removes first n elements from a list.
      ```
      GHCi> take 5 [2,4..100]
      [2,4,6,8,10]

      GHCi> take 3 "wonderful"
      "won"

      GHCi> take 1000000 [1]
      [1]
      ```
      > combine take and reverse to take last n elements from a list
      ```
      takeLast n aList = reverse (take n (reverse aList))

      GHCi> takeLast 10 [1..100]
      [91,92,93,94,95,96,97,98,99,100]
      ```
      > drop in action
      ```
      GHCi> drop 2 [1,2,3,4,5]
      [3,4,5]

      GHCi> drop 5 "very awesome"
      "awesome"
      ```
   10. `zip` to combine two lists into tuple pairs. Inputs are 2 lists. Output is a list [].
         ```
         GHCi> zip [1,2,3] [2,4,6]
         [(1,2),(2,4),(3,6)]

         GHCi> zip "dog" "rabbit"
         [('d','r'),('o','a'),('g','b')]

         GHCi> zip ['a' .. 'f'] [1 .. ]
         [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
         ```
   11. `cycle` repeats a list endlessly
         ```
         ones n = take n (cycle [1])

         GHCi> ones 4
         [1,1,1,1]
         ```
   12. **Useful** assigning people to teams
         ```
         assignToGroups n aList = zip groups aList
                  where groups = cycle [1..n]

         GHCi> assignToGroups 3 ["file1.txt","file2.txt","file3.txt" ,"file4.txt","file5.txt","file6.txt","file7.txt","file8.txt"]
         [(1,"file1.txt"),(2,"file2.txt"),(3,"file3.txt"),(1,"file4.txt"),(2,"file5.txt"),(3,"file6.txt"),(1,"file7.txt"),(2,"file8.txt")]

         GHCi> assignToGroups 2 ["Bob","Kathy","Sue","Joan","Jim","Mike"]
         [(1,"Bob"),(2,"Kathy"),(1,"Sue"),(2,"Joan"),(1,"Jim"),(2,"Mike")]
         ```
6. Ch07:
   1. No *for*, *while* and *until* loops in functional language because dont have state changes. Solution is recursion using pattern matching. Recursive -> defined in terms of itself.
   2. Recursion is merely a list of goals and alternative cases. Think in patterns.
   3. Recursive functions rules:
      1. Identify the end goal(s). e.g. empty sink in dishwashing; may have 2 ends e.g make 100 calls or 5 sales for each day.
      2. Determine what happens when a goal is reached. ie "What happens if the function is called on the goal state value?"
      3. List all alternate possibilities one at a time.  most of the time you have only one or two alternatives to being in the goal state. If you don’t have an empty list, you have a list with something in it.
      4. Determine your “rinse and repeat” process. e.g For a list, you might take the element and look at the tail.
      5. Ensure that each alternative moves you toward your goal.
   4. Write recursive functions
      1. `if then else` expressions or
      2. `case` expressions with pattern matching.
         1. **Order of case is important** bcos results will output the first match.
         2. _ as a wildcard for catch all other cases. `_` means for anything (else) as in `myTail (_:xs) = xs`
   5. Convention for list
      1. `x` for element
      2. `xs` for list of values
   6. `error` function to throw an error
      ```
      myHead (x:xs) = xmyHead [] = error "No head for empty list"
      ```
7. Ch08:
   1. `:set +s` to time your function calls in prelude
   2. Sometimes the 5 recursion rules dont apply. e.g cycle has no goat state.
   3. Ackermann function
      ```
      ackermann 0 n = n + 1
      ackermann m 0 = ackermann (m-1) 1
      ackermann m n = ackermann (m-1) (ackermann m (n-1))
      ```
   4. collatz. The basis is number by applying collatz can get to 1.
      ```
      collatz 1 = 1
      collatz n = if even n
                  then 1 + collatz (n `div` 2)
                  else 1 + collatz (n*3 + 1)
      ```
   5. Fibonacci with Q8.2
8. Ch09:
   1. Higher-order function is technically any function that takes another function as an argument.
   2. The map function takes another function and a list as arguments and applies that function to each element in the list.
      1. Map examples
      ```
      GHCi> map reverse ["dog","cat", "moose"]
      ["god","tac","esoom"]

      GHCi> map head ["dog","cat", "moose"]
      "dcm"

      GHCi> map (take 4) ["pumpkin","pie","peanut butter"]
      ["pump","pie","pean"]

      GHCi> map ("a "++) ["train","plane","boat"]
      ["a train","a plane","a boat"]

      GHCi> map (^2) [1,2,3]
      [1,4,9]
      ```
      2. Result is a new list back that’s exactly the same size as the one you put in.
      3. Removing the concatenating and squaring function, replacing them with an f for any function, you end up with the definition of map.
      > end state
      ```
      addAnA [] = []
      ```
      > alternative of non-empty list
      ```
      addAnA (x:xs) = ("a " ++ x):addAnA xs
      ```
      > Replacing the concatenation and squaring function with an `f` for any function, you will get the definition of `map`
      ```
      squareAll [] = []
      squareAll (x:xs) = x^2:squareAll xs


      myMap f [] = []
      myMap f (x:xs) = (f x):myMap f xs
      ```

   3. Filter : similar to `map` : taking a function and a list as arguments and returning a list. The difference is that the function passed to filter must be passed a function that returns True or False. The filter function works by keeping only the elements of the list that pass the test. As with map, the goal of filter is an empty list. What makes filter different is that there are two possible alternatives:
      1. a nonempty list in which the first element passes, and
      2. a nonempty list in which the first element doesn’t pass. The only difference is that when the test fails, the element isn’t recursively consed to the list.

      ```
      GHCi> filter even [1,2,3,4]
      [2,4]

      GHCi> filter (\(x:xs) -> x == 'a') ["apple","banana","avocado"]
      ["apple","avocado"]
      ```
   4. Foldl :  takes three arguments: a binary function, an initial value, and a list. foldl works is to apply the binary argument to the initial value and the **head** of the list.
      > Vizualizing foldl (+)
      ```
      foldl (+) 0 [1,2,3,4]
                0 + 1 = 1  -- applying function to the head of list

      foldl (+) 1 [2,3,4]
                1 + 2 = 3  -- applying function to the head of list

      foldl (+) 3 [3,4]
                3 + 3 = 6  -- applying function to the head of list

      foldl (+) 6 [4]
                6 + 4 = 10 -- applying function to the head of list

      foldl (+) 10 [] = 10
      ```
      1. Initial value for `foldl (+)` = 0 => mySum xs = foldl (+) 0 xs
      2. Initial value for `foldl (*)` = 1 => myProduct xs = foldl (*) 1 xs
      3. Initial value for concat with `foldl (++)` = blank => concatAll xs = foldl (++) "" xs
      4. Initial value for list work like reverse list `foldl rcons [] xs where rcons x y = y : x` is an empty list [].
      5. Common foldl and map combo:  e.g `sumOfSquares` is squares every value in a list and then takes the sum of it:
      ```
      sumOfSquares xs = foldl (+) 0 (map (^2) xs)
      ```
      1. Reverse a list with foldl. Use a helper function `rcons` for consing elements in the reverse order.
      ```
      rcons x y = y:x
      myReverse xs = foldl rcons [] xs
      ```
      > visualizing
      ```
      foldl rcons [] [1,2,3]
                1:[]

      foldl rcons [1] [2,3]
                2:[1]

      foldl rcons [2,1] [3]
                3:[2,1]

      foldl rcons [3,2,1] [] = [3,2,1]
      ```
      1. foldl end goal state is still the empty list []. Because the initial value will get updated after each call to the binary function, it’ll contain the final value in your computation.
      2. When you reach the end of the list, you return the **current** , **NOT original init** value for init:
      ```
      myFoldl f init [] = init
      ```
      You have only one other alternative: a nonempty list. For this, you pass your initial value and the head of your list to the binary function. This creates your new init value. Then you call myFoldl on the rest of the list by using this new value as your init:
      ```
      myFoldl f init [] = init
      myFoldl f init (x:xs) = myFoldl f newInit xs
         where newInit = f init x
      ```

   5. Foldr: two arguments in a binary function: a left argument and a right argument. The left fold compacts the list into the left argument, and the right fold into the right argument
      ```
      myFoldr f init [] = init
      myFoldr f init (x:xs) = f x rightResult
         where rightResult = myFoldr f init xs


      foldr (-) 0 [1,2,3,4] -- = (1 - (2 - (3 - (4 - 0))))
      ```
   6. Folds summary:
      1. foldl is the most intuitive behaving of the folds, but it usually has terrible perfor-mance and can’t be used on infinite lists.
      2. foldl' is a nonlazy version of foldl that’s often much more efficient.
      3. foldr is often more efficient than foldl and is the only fold that works on infinite lists
9. Ch11:
   1. Type Int and Integer are different in Haskell
      1. Type Int is bound
      ```
      x :: Int
      x = 2
      GHCi> x*2000
      4000
      GHCi> x^2000
      0
      ```
      2. Type Integer is not bounded by memory limitations framed in terms of bytes. Like any whole number.
      ```
      y :: Integer
      y = 2
      GHCi> y*2000
      4000
      GHCi> y^2000
      114813069527425452423283320117768198402231770208869520047764273682576626139237031385665948631650626991844596463898746277344711896086305533142593135616665318539129989145312280000688779148240044871428926990063486244781615463646388363947317026040466353970904996558162398808944629605623311649536164221970332681344168908984458505602379484807914058900934776500429002716706625830522008132236281291761267883317206598995396418127021779858404042159853183251540889433902091920554957783589672039160081957216630582755380425583726015528348786419432054508915275783882625175435528800822842770817965453762184851149029376
      ```
   2. List Types
      ```
      values :: [Int]
      values = [1,2,3]

      testScores :: [Double]
      testScores = [0.99,0.7,0.8]

      letters :: [Char]
      letters = ['a','b','c']
      ```

   3. A list of characters `[Char]` is the same as a string `String` :
      ```
      GHCi> letters == "abc"
      True
      ```
   4. Tuples can contain mulitple types. A list of type [Char] is a string of any size, whereas a tuple of type (Char) is a tuple of exactly one character.
      ```
      manyItems :: (Int, Double, String, Char) -- or (Int, Double, [Char], Char)
      manyItems = (24,  0.99, "Haskell", 'H')
      ```
   5.  Half-ing correctly. Casting forces a value to be represented as a different type. Haskell has no convention for casting types and instead relies on functions that properly transform values from one type to another -- `fromIntegral`.
      ```
      half :: Int -> Double
      half n = n/2   <--- Incorrect code!

      half n = (fromIntegral n) / 2   <--- Correct code!
      ```
   6. Convert values to and from strings with `show` and `read`.
      1. > show to make into a string.
      ```
      GHCi> show 6
      "6"
      GHCi> show 'c'
      "'c'"
      GHCi>show 6.0
      "6.0"
      ```
      2. > read to convert a string to another type
      ```
      z = read "6"
      q = z / 2  --- and Haskell will know to treat z as a `Double` .
      ```
      3. 2 ways to write return type.
         1. As a type signature
         ```
         anotherNumber :: Int
         anotherNumber = read "6"
         ```
         2. Append the expected return type to the end of a function call, most frequently in GHCi.
         ```
         GHCi> read "6" :: Int
         6
         GHCi> read "6" :: Double
         6.0
         ```
   7. Order from most to least general
   8. Multi-argument functionas a sequence of nested lambda function
      1. > makeAddress number street town = (number, street, town)
      ```
      makeAddressLambda = (\number ->
                              (\street ->
                                 (\town -> (number, street, town)))

      GHCi> (((makeAddressLambda 123) "Happy St") "Haskell Town")
      (123,"Happy St","Haskell Town")

      GHCi> (((makeAddress 123) "Happy St") "Haskell Town")
      (123,"Happy St","Haskell Town")

      GHCi> makeAddressLambda 123 "Happy St" "Haskell Town"
      (123,"Happy St","Haskell Town")
      ```
   9. Types for first class functions aka functions that take functions as arguments and return functions as values. These sub functions arguments types are written in ( ) e.g. (Int -> Int)
      ```
      ifEven :: (Int -> Int) -> Int -> Int
      ifEven f n = if even n
                  then f n
                  else n
      ```
   10. `simple` function always return itself there its type signature has input argument type gives output return type. Haskell has type variables. **Any lower-case letter in a type signature indicates that any type can be used in that place.** Type variables are literally variables for types. Type variables work exactly like regular variables, but instead of representing a value, they represent a type.

      ```
      simple :: a -> a
      simple x = x
      ```
   11. **All types of the same variable name must be the same ie consistency.**
   12. **Using different names for type variables doesn’t imply that the values represented by the variables must be different, only that they can be.**
      ```
      f1 :: a -> a
      f2 :: a -> b
      ```
      > f2 is a function that can produce a much wider range of possible values. The f1 function could behave only by changing a value and keeping it as the same type: Int -> Int, Char -> Char, and so forth. In contrast, f2 can represent a much broader range of possible behaviors: Int -> Char, Int -> Int, Int -> Bool, Char -> Int, Char -> Bool, and so forth.

   13. See exercises for 11.3
10. Ch12.0
    1.  [Char] = `String` --> Type synonym
    2.  Type synonyms: 2 types
        1.   1:1 replacement
         ```
         type FirstName = String
         type LastName = String
         type Age = Int
         type Height = Int


         patientInfo :: String -> String -> Int -> Int -> String
         patientInfo fname lname age height = name ++ " " ++ ageHeight
               where name = lname ++ ", " ++ fname
                     ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

         GHCi> patientInfo "Jane" "Smith" 25 62
         "Smith, Jane (25yrs. 62in.)"
         ```

         > rewriting type signature for type synonym

         ```
         patientInfo :: FirstName -> LastName -> Age -> Height -> String
         ```

        2. Not 1:1 - Combine type synonym then use helper function

         ```
         type PatientName = (String,String)

         firstName :: PatientName -> String
         firstName patient = fst patient

         lastName :: PatientName -> String
         lastName patient = snd patient
         ```

    3. Type definition with `data`
         ```
         data Sex = Male | Female

         `Sex` : type constuctor
         `Male | Female` : - data constructors RHS ie values ~ True and False
                           - The Sex type is an instance of either of these data constructors.

         -- Another e.g
         data Shape = Circle Radius | Square Length | Rectange Length Breadth --deriving Show
         type Radius = Double -- note without type constructor name as solo Double is a "major type"
         ```
         The steps are:
         1. `data` : to initiate a new (data) type
         2. Specify the type constructor e.g `Sex` capitalise 1st letter. Type constructor = name of the type.
         3. Write all the data constructors e.g. both `Male` and `Female`. A data constructor is used to create a concrete instance of the type.
         4. Separate the data constructors with `|` to create the various instances. The Sex type can be either Male or an instance of Female.
         5. Ready to create a function with the `Sex` type as one of the argument type.
            ```
            sexInitial :: Sex -> Char
            sexInitial Male = 'M'
            sexInitial Female = 'F'

            -- Another example
            data Patient = Patient Name Sex Int Int Int BloodType
            ```


    4. Readability is impt instead of Male :: Sex -> Bool, opt for "M" or "F"
         ```
         sexInitial :: Sex -> Char
         sexInitial Male = 'M'
         sexInitial Female = 'F'
         ```
       1. Blood type ABO (A, B, AB, O) + - would create 8 data constructors. Instead go for Rh and ABO types as distinct.
         ```
         data RhType = Pos | Neg

         data ABOType = A | B | AB | O
         ```
       2.  With Rh and ABO types as distinct, BloodType is a combo of both
         ```
         data BloodType = BloodType ABOType RhType

         -- RHS BloodType is a data constructor, making it the same name as type constructor.
         -- Its job is combine `ABOType` and `RhType`
         -- This means a BloodType is an ABOType with an RhType.
         -- Read more in unit2/lesson12/createTypesRecordSyntax.hs

         data Name = Name FirstName LastName  -- RHS Name is a data constructor
                   | NameWithMiddle FirstName MiddleName LastName   -- NameWithMiddle is a data constructor
         ```

    5. *Record syntax* Go thru the process. Step 1 could get painful if `Patient` uses 12 values to define the type
       1. To get each value of the individual patient, use functions to get each value using pattern matching.
        ```
         -- Recall:
         data Patient = Patient Name Sex Int Int Int BloodType

         getName :: Patient -> Name
         getName (Patient n _ _ _ _ _) = n

         getAge :: Patient -> Int
         getAge (Patient  _ _ a _ _ _) = a

         getBloodType :: Patient -> BloodType
         getBloodType (Patient _ _ _ _ _ bt) = bt
        ```
       2. Allow easy data creation bcos each field can be set by name and order is irrelevant. See unit2/lesson12/createTypesRecordSyntax.hs here
        ```
         jackieSmith :: Patient'
         jackieSmith = Patient' {name = Name "Jackie" "Smith"
                        , age = 43
                        , sex = Female
                        , height = 62
                        , weight = 115
                        , bloodType = BloodType O Neg }
         ```
       3. Access a field value from the record
         ```
         GHCi> height jackieSmith
         62
         GHCi> showBloodType (bloodType jackieSmith)
         "O-"
         ```
       4. Allow update fields easily
         ```
         jackieSmithUpdated :: Patient'
         jackieSmithUpdated = jackieSmith { age = 44 }
         ```
11. Ch13.0
    1.  Type classes allow you to group types based on shared behavior. A type class states which functions a type must support and is a way of describing groups of types that all behave in the same way
    2. `Num a` means some type `a` of class `Num`. Use `:info` to get definition. All members of cthe class must implement these functions, where output type and argument types are the same such that you cannot add two `Int`s  nand get a `Double`.
         ```
         GHCi> :info Num
         class Num a where
            (+) :: a -> a -> a
            (-) :: a -> a -> a
            (*) :: a -> a -> a
            negate :: a -> a
            abs :: a -> a
            signum :: a -> a  -- ie gives the sign of a number
         ```
    3.  Nearly all functions required in any type class definition will be expressed in terms of type variables, because otherwise the functions will be tied to a single type.
    4.  Type class definition structure:
         ```
         class TypeName a where
            fun1 :: a -> a
            fun2 :: a -> String
            fun3 :: a -> a -> Bool

         TypeName: Name of the type class
         fun1, fun2, fun3: Names of all the required functions
         a -> a / a -> String / a -> a -> Bool:  Type signatures of the required functions
         a in class TypeName a where ...: Type variable as a placeholder for the specific type that willimplement this class
         ```
    5.  Example: A class `Describable` with an instance that always return a string for whatever type the argument. Write the function `describe`to call on an instance that will return a string. For whatever type you have, if it’s `Describable`, calling `describe` on an instance of the type will tell you all about it.
         ```
         GHCi> describe False
         "A member of the Bool class, False is the opposite of True"

         Answer:
         class Describable a where
            describe :: a -> String
         ```
    6.  `Ord` for things that can be compared and ordered.
      >  Take any two of the same types that implement Ord, and return a Boolean
         ```
         GHCi> :t (>)
         (>) :: Ord a => a -> a -> Bool
         ```
    7.  `Ord` class needs `Eq` class which only needs 2 functions. Ord type class includes the Eq type class in its definition. but not everything can be ranked e.g vanilla vs choco ice cream which can do `Eq` but not `Ord`.
         ```
         class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool

         class Eq a => Ord a where
            compare :: a -> a -> Ordering
            (<) :: a -> a -> Bool
            (<=) :: a -> a -> Bool
            (>) :: a -> a -> Bool
            (>=) :: a -> a -> Bool
            max :: a -> a -> a
            min :: a -> a -> a
         ```
    8.  Use `:info` to check the  type classess membership. e.g `:info Int` to know Int belongs to Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show.
    9. `Bounded`. minBound and maxBound are values, not functions. `Char` and `Int` are members of the `Bounded` type class.
         ```
         class Bounded a where
            minBound :: a
            maxBound :: a

         GHCi> minBound :: Int
         -9223372036854775808

         GHCi> maxBound :: Int
         9223372036854775807

         GHCi> minBound :: Char
         '\NUL'
         ```
    10.  Add `deriving (Show)` at the end after data constructors to get data constructors values to show , similarly to Bool which is a member of the `Show` type class (Check with :info Bool).
         ```
         data Icecream = Chocolate | Vanilla

         Bool are member of `Show`
         GHCi> True
         True
         GHCi> False
         False

         GHCi> Chocolate --error
         <interactive>:404:1:No instance for (Show Icecream) arising from a use of ‘print’

         Rectify with
         data Icecream = Chocolate | Vanilla deriving (Show)

         GHCi> Chocolate
         Chocolate
         ```
    11.  Common to add other type class `deriving (Show, Eq, Ord)` also. Haskell defaults to the order of the data constructors for determining Ord. So Vanilla will be greater than Chocolate.
         ```
         data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

         GHCi> Vanilla == Vanilla
         True
         GHCi> Chocolate == Vanilla
         False
         GHCi> Chocolate /= Vanilla
         True

         GHCi> Chocolate > Vanilla
         False
         ```
12. Ch14.0
    1.  Functions in type classes are called *methods*.
    2.  Unless it is a type definition with show, add `deriving (Show)`.
    3.  But to intro other ( corresponding string) output beyond data constructors, specify `data` constructor to define type definition and `instance Show type where show data-constructor-value = "I" ` etc. See SixSidedDie in /Users/regina/Code/Haskell/GetProgrammingWithHaskell/unit2/lesson14/typeClassCreation.hs
    4.  **Polymorphism** means that the same function behaves differently depending on the type of data it’s working with. e.g `read`
         ```
         read "10"

         For read :: Int
         read "10" returns 10

         For read :: Double
         read "10" returns 10.0
         ```
    5. **Superclass** : `Eq` is a superclass of `Ord` means that every instance of `Ord` must also be an instance of `Eq`. e.g  to compare SixSided-Die data constructors, which means implementing Ord, so first you need to implement Eq
         ```
         class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool
         ```
    6. Type classes can have default implementations of methods. If you define (==), Haskell can figure out what (/=) means without any help. e.g
         ```
         instance Eq SixSidedDie where
            (==) S6 S6 = True
            (==) S5 S5 = True
            (==) S4 S4 = True
            (==) S3 S3 = True
            (==) S2 S2 = True
            (==) S1 S1 = True
            (==) _ _ = False
         ```
    7. Use `:info` for info but it is incomplete. For minimal requirement for implementation look at *Hackage* -> Type Class (e.g Eq (https://hackage.haskell .org/package/base/docs/Data-Eq.html) -> "**Minimum complete definition**" e.g Eq (==) | (/=). To implement the Eq type class, all you have to define is either (==) or (/=). With either one of these options, Haskell can work out the rest for you. Hoogle is the interface for Hackage. Hoogle can be found at www.haskell.org/hoogle.
    8. About `Ord`. Suffice to just implement `compare`. Functions for `Ord` :
         ```
         class Eq a => Ord a where
            compare :: a -> a -> Ordering  -- ** only need to implement compare
            (<) :: a -> a -> Bool
            (<=) :: a -> a -> Bool
            (>) :: a -> a -> Bool
            (>=) :: a -> a -> Bool
            max :: a -> a -> a
            min :: a -> a -> a

         -- Recall that ordering is used in `sort` in lesson 4
         data Ordering = LT | EQ | GT
         ```
    9. To shortcut listing all the combi for `Ord` in `Ord SixSidedDieEq` in typeClassCreation.hs, use `deriving (Ord)` since order of data constructors will determine their priority.
         ```
         data Test1 = AA | ZZ deriving (Show, Eq, Ord)
         data Test2 = ZZZ | AAA deriving (Show, Eq, Ord)

         GHCi> AA < ZZ
         True
         GHCi> AA > ZZ
         False
         GHCi> AAA > ZZZ
         True
         GHCi> AAA < ZZZ
         False
         ```
    10. About `Enum`. `Enum` type allows you to represent your dice sides as an enumerated list of constants. See Enum class but actually just `fromEnum` and `toEnum` will do so that now SixSidedDie can now be treated as `Int` and `C  NB: to start with 0.
         ```
         class Enum a where
            succ :: a -> a
            pred :: a -> a
            toEnum :: Int -> a
            fromEnum :: a -> Int
            enumFrom :: a -> [a]
            enumFromThen :: a -> a -> [a]
            enumFromTo :: a -> a -> [a]
            enumFromThenTo :: a -> a -> a -> [a]

            instance Enum SixSidedDie where
               toEnum 0 = S1
               toEnum 1 = S2
               toEnum 2 = S3
               toEnum 3 = S4
               toEnum 4 = S5
               toEnum 5 = S6
               toEnum _ = error "No such value"

               fromEnum S1 = 0
               fromEnum S2 = 1
               fromEnum S3 = 2
               fromEnum S4 = 3
               fromEnum S5 = 4
               fromEnum S6 = 5

            -- Test to see that now the data constructors of SixSidedDie is now number
            GHCi> [S1 .. S6]
            [one,two,three,four,five,six]
            GHCi> [S2,S4 .. S6]
            [two,four,six]
            GHCi> [S4 .. S6]
            [four,five,six]
            GHCi> [S1 .. ]
            [one,two,three,four,five,six,*** Exception: No such value  -- didnt handle the case of missing value.
            -- Put an end to the list by defining the type constructors with deriving (Enum):

            data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)
            GHCi> [S1 .. ]
            [one,two,three,four,five,six]
         ```
    11. Rem the sort names sort by first name and was overcome by sortBy with a first class function to sort by second name. `type Name = (String, String) names :: [Name]`, we have to implement my own custom `Ord` for Name by (re)defining `compare` AND refine Name to include deriving (Show, Eq) to show "This tuple is special from others."
         ```
         data Name = Name (String, String) deriving (Show, Eq) vs the typical type Name = (String, String)

         instance Ord Name where
            compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

         names :: [Name]
         names = [Name ("Emil","Cioran")
                  , Name ("Eugene","Thacker")
                  , Name ("Friedrich","Nietzsche")]

         -- Now your names are sorted as expected:
         GHCi> import Data.List
         GHCi> sort names
         [Name ("Emil","Cioran"),Name ("Friedrich","Nietzsche"),➥Name ("Eugene","Thacker")]
         ```
    12. **newtype** can replace `data` but not vice versa. `newtype` can have only one type constructor and one type (in the case of Name above,it’s Tuple -- Name(String, String)). **For simplicity, we’ll stick to creating types with data throughout this book.**
    13.  Arrows from one class to another indicate a superclass relationship. Type class road map ![Alt text](unit2/lesson14/TypeclassRoadMap.png?raw=true "Type Class Road Map")
    14.  Note minimum complete definition when creating `instance Typeclass MyType where...`. PLEASE See unit2/lesson14/l14exercises.hs
    15. Define a type class with function and then an instance of it.
          ```
            --Q14.2 (Ans)
            -- qn : define a five-sided die
            data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show) -- Dont need Ord as Eq is a superclass of Ord

            -- qn: define a type class named Die with a useful method for a die. Include a superclass (chose Eq as superclass of Ord which die must have)
            class (Eq a, Enum a) => Die a where
            roll :: Int ->  a

            -- qn: make your FiveSidedDie an instance of Die
            instance Die FiveSidedDie where
            roll n = toEnum (n `mod` 5)
           ```
13. Ch16.0
    1. Algebraic data types = any types that can be made by combining other types, only 2 types
       1. Product types (using `and` as in a name is a String and another String)
       2. Sum types (using `or` as in  Bool which is a True data constructor or a False data constructor).
    2. Product types are created by combining two or more existing types with *and* .e.g
       1. fraction : numerator (`Integer`) *and* a denominator (`Integer`). NB `Integer` is infinite range of number but (-2 ^ 29) < `Int` < ((2 ^ 29) -1)
       2. street address: number (`Int`) *and* a street name (`String`)
       3. mailing address: street name (`String`) *and* a city (`String`) *and* a state (`String`) *and* a zip code (`Int`)
    3. Sum types are created by combining two or more existing types with *or* e.g
       1. Bool or Name, Creator, Artist in below. `Artist`, `Author`, and as a result, `Creator` all depend on the definition of `Name`. But you had to change only the definition of `Name` itself and didn’t need to worry at all about how any other types using `Name` are defined. With Sum type, creating groups of similar types is convenient (e.g StoreItem which is a book/record/collectible toy.)
         ```
         type FirstName = String
         type LastName = String
         type MiddleName = String

         -- Sum type example 1
         data Name = Name FirstName LastName                     -- RHS Name is a type constructor
                  | NameWithMiddle FirstName MiddleName LastName -- NameWithMiddle is a type constructor consisting of 3 strings
                  | TwoInitialsWithLast Char Char LastName       --for edge case e.g H.P. Lovecraft
                  | FirstNameWithTwoInits FirstName Char Char    -- more edge case
                  deriving (Show)         -- needed if going to create other types with data constructors that use `Name`. See unit3/lesson16/productNSumTypes.hs quick check 16.2 `show (author book)` in fn `madeBy` when this `deriving (Show)` is removed.

         -- Sum type example 2
         data Creator = AuthorCreator Author
                     | ArtistCreator Artist

         newtype Author = Author Name

        -- Sum type example 3
         data Artist = Person Name | Band String

         -- so this makes creating a creator with 2 initials & a last name
         hpLovecraft :: Creator
         hpLovecraft = AuthorCreator
                           (Author
                              (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

        -- Sum type example 4
        data StoreItem = BookItem Book
                        | RecordItem VinylRecord
                        | ToyItem CollectibleToy
         ```
    4. Trap of record syntax. See unit3/lesson16/productNSumTypes.hs for Book (bookPrice) vs VinylRecord (recordPrice). In record syntax, we cannot reuse the same name ie price otherwise we have duplicate function name for different purposes.
         ```
         -- without record syntax
         data Book = Book Creator String String Int Double

         -- record syntax style (book)
         price :: Book -> Double
         price (Book _ _ _ _ val) = val

         -- record syntax style (vinylRecord)
         price :: VinylRecord -> Double
         price (VinylRecord _ _ _ val) = val

         The problem is that using the same name for a property of both a Book and a VinylRecord means defining conflicting functions!

         ```
    5. To access attribute of a any StoreType type of which all has its prices with a different label (see unit3/lesson16/productNSumTypes.hs. See also unit3/lesson16/l16exercises.hs -> Rem to add `deriving (Show)` when creating fresh types that uses my new created types e.g. `Name`, `Creator` ..etc
         ```
         price :: StoreItem -> Double
         price (BookItem book) = bookPrice book
         price (RecordItem record) = recordPrice record
         price (ToyItem toy) = toyPrice toy
         ```