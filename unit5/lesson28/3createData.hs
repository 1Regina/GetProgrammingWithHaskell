
data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

-- Scenario 1 (non-Maybe context)
-- GHCi> User {name = "Sue", gamerId = 1337, score = 9001}
-- User {name = "Sue", gamerId = 1337, score = 9001}
-- GHCi> User "Sue" 1337 9001
-- User {name = "Sue", gamerId = 1337, score = 9001}
-- same 2 Methods
-- >>> User { name = "Sue", gamerId = 1337, score = 9001 }
-- User {name = "Sue", gamerId = 1337, score = 9001}
-- >>> User "Sue" 1337 9001
-- User {name = "Sue", gamerId = 1337, score = 9001}
-- same 2 methods
-- sue :: User
-- sue = User "Sue" 1337 9001    -- method 1
-- sue = User
--      { name    = "Sue"
--      , gamerId = 1337
--      , score   = 9001 }    -- method 2

-- Scenario 2 (Maybe context)
serverUsername :: Maybe String
serverUsername = Just "Sue"
serverGamerId :: Maybe Int
serverGamerId =  Just 1337
serverScore :: Maybe Int
serverScore = Just 9001

-- >>> User <$> serverUsername <*> serverGamerId <*> serverScore
-- Just (User {name = "Sue", gamerId = 1337, score = 9001})


readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user

-- Quick check 28.5    Show the result of creating a user with a missing (Nothing) userName
-- >>> User <$> Nothing <*> Just 1234 <*> Just 0000
-- Nothing

-- >>> User <$> Nothing <*> serverGamerId <*> serverScore
-- Nothing
