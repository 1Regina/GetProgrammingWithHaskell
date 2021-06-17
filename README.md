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
   2. Anytime you might want to use a closure (which in Haskell is pretty much anytime), you want to order your arguments from most to least general. Closures combine lambda functions and first-class functions to give you amazing power but partial application makes life even easier.
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
      GHCi>  'h':[]:'e':'l':'l':'o':[]  -- so impossible
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
   12. **Useful**
      ```
      assignToGroups n aList = zip groups aList
               where groups = cycle [1..n]
      GHCi> assignToGroups 3 ["file1.txt","file2.txt","file3.txt" ,"file4.txt","file5.txt","file6.txt","file7.txt","file8.txt"]
      [(1,"file1.txt"),(2,"file2.txt"),(3,"file3.txt"),(1,"file4.txt"),(2,"file5.txt"),(3,"file6.txt"),(1,"file7.txt"),(2,"file8.txt")]
      GHCi> assignToGroups 2 ["Bob","Kathy","Sue","Joan","Jim","Mike"]
      [(1,"Bob"),(2,"Kathy"),(1,"Sue"),(2,"Joan"),(1,"Jim"),(2,"Mike")]
      ```
6. Ch06:
   1.