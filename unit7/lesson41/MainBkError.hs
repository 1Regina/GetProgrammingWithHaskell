module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time


main :: IO ()
main = do
     print "Enter a command"
     command <- getLine
     performCommand command


data Tool = Tool
        { toolId :: Int
        , name :: String
        , description :: String
        , lastReturned :: Day
        , timesBorrowed :: Int
        }


data User = User
        { userId :: Int
        , userName :: String
        }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".)  "
                        , userName user]


instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"]


-- 1. Create new data after create database
-- addUser :: String -> IO ()
-- addUser userName = do
--     conn <- open "tools.db"
--     execute conn "INSERT INTO users (username) VALUES (?)"
--             (Only userName)
--             print "user added"
--     close conn

-- abstract out connecting to db
withConn :: String -> (Connection -> IO()) -> IO()
withConn dbName action = do
        conn <- open dbName
        action conn
        close conn


-- Quick check 41.2 Refactor addUser to use withConn.
addUser :: String -> IO()
addUser username = withConn "tools.db" $
                \conn -> do
                        execute conn "INSERT INTO users (username) VALUES (?)"
                        (Only userName)
                        print "user added"

-- create a checkout tool by user Id
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                                 execute conn
                                 "INSERT INTO checkout (user_id, tool_id) VALUES (?, ?)"
                                 print (userId, toolId)

-- fromRow method returns a RowParser of type a, where a is the same type as whatever type you’re making an instance of FromRow
-- class FromRow a where
--            fromRow :: RowParser a


-- toRow barely used. ToRow is much less useful, because it trans-forms your data types into a tuple of values. SQLText and SQLInteger constructors transform Haskell Text and Integer types to SQLdata.
instance ToRow Tool where
           toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
                        , SQLText $ T.pack $ name tool
                        , SQLText $ T.pack $ description tool
                        , SQLText $ T.pack $  show $ lastReturned tool
                        , SQLInteger $ fromIntegral $ timesBorrowed tool ]


-- a function from SQLite.Simple called field - used internally by SQLite.Simple to consume the data from a row and transform it into the values used by your type constructors.
instance FromRow User where
        fromRow = User <$> field
                       <*> field

instance FromRow Tool where
        fromRow = Tool <$> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field

--`query` and `query_` to query data
-- query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
-- query_ :: FromRow r => Connection -> Query -> IO [r]


--2. READ
printUsers :: IO ()
printUsers = withConn "tools.db" $
             \conn ->  do
                 resp <- query_ conn "SELECT * FROM users;" :: IO [User]
                 mapM_ print resp

-- >>> addUser "test user"
-- >>> printUsers
-- from book p534
-- GHCi> printUsers
-- 1.)  willkurt
-- GHCi> addUser "test user"
-- "user added"
-- GHCi> printUsers
-- 1.) willkurt
-- 2.) test user


printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
                       \conn ->  do
                         resp <- query_ conn q :: IO [Tool]
                         mapM_ print resp

printTools :: IO ()
printTools =  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from➥checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
                  mconcat [ "select * from tools "
                          , "where id in "
                          , "(select tool_id from checkedout);"]

 -- from book p535
--  GHCi> printTools
--  1.) hammer
--  description: hits stuff
--  last returned: 2017-01-01
--  times borrowed: 0

--  2.) saw
--  description: cuts stuff
--  last returned: 2017-01-01
--  times borrowed: 0
--  GHCi> checkout 1 2
--  GHCi> printCheckedOut
--  2.) saw
--  description: cuts stuff
--  last returned: 2017-01-01
--  times borrowed: 0

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
        resp <- query conn
                "SELECT * FROM tools WHERE id = (?)"
                (Only toolId) :: IO [Tool]
        return $ firstOrNothing resp
-- firstOrNothing function looks at the list of results returned by your query. If the list is empty, it returns Nothing; if you have results (presumably just one, because the ID is unique), it returns the first one.

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

-- 3. Update
-- updateTool function takes an existing tool and returns a new tool with an updated lastReturned date and timesBorrowed count
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    {   lastReturned = date
       , timesBorrowed = 1 + timesBorrowed tool
    }

--  the tool is a Maybe Tool, you want your code to update your table only if the Maybe value isn’t Nothing
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =  withConn "tools.db" $
                            \conn -> do
                                    let q = mconcat ["UPDATE TOOLS SET  "
                                                     ,"lastReturned = ?,"
                                                     ," timesBorrowed = ? "
                                                     ,"WHERE ID = ?;"]
                                    execute conn q (lastReturned tool
                                                      , timesBorrowed tool
                                                      , toolId tool)
                                    print "tool updated"

-- updateToolTable, takes a toolId, fetches the current date, and then performs the necessary steps to update the tool in the table

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                    \conn -> do
                       tool <- selectTool conn toolId
                       currentDay <- utctDay <$> getCurrentTime
                       let updatedTool = updateTool <$> tool <*> pure currentDay
                       updateOrWarn updatedTool

--4. Delete
checkin :: Int -> IO ()
checkin toolId =  withConn "tools.db" $
                        \conn -> do
                            execute conn
                                "DELETE FROM checkedout WHERE tool_id = (?);"
                                (Only toolId)

-- 3. Check and update
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
        checkin toolId
        updateToolTable toolId

--5. Parts for an interface
promptAndAddUser :: IO ()
promptAndAddUser = do
        print "Enter new user name"
        userName <- getLine
        addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
        print "Enter the id of the user"
        userId <- pure read <*> getLine
        print "Enter the id of the tool"
        toolId <- pure read <*> getLine
        checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
        print "enter the id of tool"
        toolId <- pure read <*> getLine
        checkinAndUpdate toolId

-- command-line interface to repeatedly prompt the user for more input until the user quits your program

performCommand :: String -> IO ()
performCommand command
        | command == "users" = printUsers >> main
        | command == "tools" = printTools >> main
        | command == "adduser" = promptAndAddUser >> main
        | command == "checkout" = promptAndCheckout >> main
        | command == "checkin" = promptAndCheckin >> main
        | command == "in" = printAvailable >> main
        | command == "out" = printCheckedout >> main
        | command == "quit" = print "bye!"
        | otherwise = print "Sorry command not found" >> main