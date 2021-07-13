
-- Possibly null values: successfulRequest and failedRequest for extraction from DB

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- when Null , value wont b written. Increase +1 only when it is an int
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

-- >>> incMaybe successfulRequest
-- Just 7
-- >>> incMaybe failedRequest
-- Nothing

-- Quick check 27.1    Write    the    function    reverseMaybe :: Maybe String -> Maybe String that reverses a Maybe String and returns it as a Maybe String

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just stringy) = Just (reverse stringy)
reverseMaybe Nothing = Nothing

-- instance Functor Maybe where
--     fmap func (Just n) = Just (func n)
--     fmap func Nothing = Nothing

-- >>>  fmap (+ 1) successfulRequest
-- Just 7
-- >>> (+1) <$>  successfulRequest
-- Just 7
-- >>> fmap (+ 1) failedRequest
-- Nothing
-- >>> (+1) <$> failedRequest
-- Nothing

-- type signature of the function in fmap is (a -> b), meaning that the Maybe returned doesn’t need to be parameterized by the same type.
-- Two examples from a Maybe Int to a Maybe String

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