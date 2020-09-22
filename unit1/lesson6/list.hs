-- >>> head [1,2,3]
-- 1
-- >>> head [[1,2],[3,4],[5,6]]
-- [1,2]
-- >>> tail [1,2,3]
-- [2,3]
-- >>> tail [3]
-- []
-- >>>  1:[]
-- [1]
-- >>> 1:2:3:4:[]
-- [1,2,3,4]
-- >>> (1,2):(3,4):(5,6):[]
-- [(1,2),(3,4),(5,6)]
-- >>> 1:[2,3,4]
-- [1,2,3,4]
-- >>> ['h','e','l','l','o']
-- "hello"
-- >>> 'h':'e':'l':'l':'o':[]
-- "hello"

-- " hello" is a list with values ie = ['h', 'e','l','l','o']
-- >>> 'h':"ello"
-- "hello"
-- >>> ['h', 'e','l','l','o']
-- "hello"

-- Error as "h" is a list. To combine, use ++
-- >>> ['h']:['e','l','l','o']
-- <interactive>:800:9-11: error:
--     • Couldn't match expected type ‘[Char]’ with actual type ‘Char’
--     • In the expression: 'e'
--       In the second argument of ‘(:)’, namely ‘['e', 'l', 'l', 'o']’
--       In the expression: ['h'] : ['e', 'l', 'l', 'o']
-- >>>  "h" ++ "ello"
-- "hello"
-- >>> [1] ++ [2,3,4]
-- [1,2,3,4]
--

-- binary function in ` ` if used as infix operator
respond :: Foldable t => t Char -> [Char]
respond phrase = if '!' `elem` phrase
                 then "wow!"                
                 else "uh.. okay"