
ifEvenCube x -- = ifEven myCube x
             = if even x
                 then x ^ 3
                else x 
            --  =  ifEven (\x -> x ^3) x

-- >>> ifEven (\x-> x^ 3) 4
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( /home/regina/smu/haskell/ProgrammingHaskell/unit1/lesson3/fnAsArguments.hs, interpreted )
-- <BLANKLINE>
-- /home/regina/smu/haskell/ProgrammingHaskell/unit1/lesson3/fnAsArguments.hs:5:14: error:
--     parse error on input ‘=’
-- Failed, no modules loaded.


-- >>> ifEvenCube 4
-- 64
--
