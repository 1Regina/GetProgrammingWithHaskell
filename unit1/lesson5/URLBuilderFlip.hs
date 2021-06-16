getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource id1 = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id1 ++
                                        "?token=" ++
                                        apiKey

-- >>> getRequestURL "http://example.com" "1337hAsk3ll" "book" "1234"
-- "http://example.com/book/1234?token=1337hAsk3ll"
--
genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host = (\apiKey resource id1 -> getRequestURL host apiKey resource id1)


exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = genHostRequestBuilder "http://example.com"

-- >>> exampleUrlBuilder "1337hAsk3ll" "book" "1234"
-- "http://example.com/book/1234?token=1337hAsk3ll"
--

genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apiKey = (\resource id1 ->
                                             hostBuilder apiKey resource id1)


myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

                    -- = genApiRequestBuilder $ (genHostRequestBuilder "http://example.com") "1337hAsk3ll"

                    -- = genApiRequestBuilder $ (getRequestURL "http://example.com" apiKey resource id1) "1337hAsk3ll"



-- >>>  myExampleUrlBuilder "book" "1234"
-- "http://example.com/book/1234?token=1337hAsk3ll"
--

myGenApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
myGenApiRequestBuilder hostBuilder apiKey resource = (\ id1 -> hostBuilder apiKey resource id1)

-- Quick Check 5.3 : Partial application specifically  for  http://example.com,  the1337hAsk3ll API key, and the book resource. That’s a function that requires only the ID of a spe-cific book and then generates the full URL
myExampleUrlBuilder1 :: [Char] -> [Char]
myExampleUrlBuilder1 id1  = "http://example.com" ++
                            "/book" ++
                            "/" ++
                            id1 ++
                            "?token=1337hAsk3ll"


myExampleUrlBuilder2 :: [Char] -> [Char]
myExampleUrlBuilder2   = getRequestURL "http://example.com" "1337hAsk3ll" "books"

substract2 :: Integer -> Integer
substract2 = flip (-) 2

-- >>> substract2 3
-- 1
--

-- >>>  (-) 2 3
-- -1
--

-- >>> flip (-) 2 3
-- 1
--
