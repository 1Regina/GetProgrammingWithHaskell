Unit 1
1. Ch02: 3 rules on haskell functions
   1. All functions must take an argument
   2. All functions must return a value
   3. Anytime a function is called with the same argument, it must return the same value. Thus python += cannot exist in Haskell
2. Ch03: lambda is graceful
   1. with where at end: sumSquareOrSquareSum :: (Num p, Ord p) => p -> p -> p
                         sumSquareOrSquareSum x y = if sumSquare > squareSum
                                                      then sumSquare
                                                      else squareSum
                                                where sumSquare = x^2 + y^2
                                                      squareSum = (x+y)^2
   2. with let - in : sumSquareOrSquareSum5 :: (Ord p, Num p) => p -> p -> p
                      sumSquareOrSquareSum5 x y = let sumSquare = (x^2 + y^2)
                                                      squareSum = ((x+y)^2)
                                                   in
                                                   if sumSquare > squareSum
                                                      then sumSquare
                                                   else squareSum
   3. lexicality: nearest is kin:
      1. add3 is always 8 regardless of y: add3 y = (\y ->
                                                      (\x -> y + x) 1 ) 3
      2. doubleCube1 is always 64 : doubleCube1 x = (\cube -> cube ** 3) (4)
      3. BUT doubleCube will change according to different x: doubleCube x = (\cube -> cube ** 3) (x * 2)