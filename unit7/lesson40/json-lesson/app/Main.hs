module Main where
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import qualified Data.Text as T
import Control.Monad -- to loop thru results for printResults

-- Aeson two key in-built functions (encode vs decode)
--1. encode :: ToJSON a => a -> ByteString
--2. decode :: FromJSON a => ByteString -> Maybe a
--2. eitherDecode :: FromJSON a => ByteString -> Either String a

-- main :: IO ()
-- main = print "hi"
-----------------------
-- 1. Converting a book type to JSON
data Book = Book
           { title :: T.Text
           , author :: T.Text
           , year :: Int
           } deriving (Show,Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {author="Will Kurt"
              ,title="Learn Haskell"
              ,year=2017}
myBookJSON :: BC.ByteString
myBookJSON = encode myBook
--------------------------
-- 2. Converting a JSON to a book type
rawJSON :: BC.ByteString
rawJSON = "{\"year\":1949,\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"
bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

theBook = Book { title = "A Short History of Decay"
            , author = "Emil Ciroan"
            , year = 1949}

rawJSON' :: BC.ByteString
rawJSON' = "{\"year\":2017,\"author\":\"Will Kurt\",\"title\":\"Learn Haskell\"}"
bookFromJSON' :: Maybe Book
bookFromJSON' = decode rawJSON'

---------------------------
-- 3. Parse failure with wrong JSON then error msg with Either
wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\":➥\"A Short History of Decay\",\"year\"=1949}"
bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

--------------------------
-- 4. Working with JSON from others
data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , errorCode :: Int
                    } deriving Show

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

-- to make your ErrorMessage type an instance of FromJSON, you need to define one function: parseJSON.
instance FromJSON ErrorMessage where
    parseJSON (Object v) =                -- (Object v) is the JSON object being parsed
        ErrorMessage <$> v .: "message"   -- ErrorMessage <$> value <*> value
                     <*> v .: "error"
-- combine <$> and <*> to safely make this ErrorMessage in the context of a Maybe
exampleMessage :: Maybe T.Text
exampleMessage = Just "Opps"
exampleError :: Maybe Int
exampleError = Just 123

-- ErrorMessage <$> value <*> value
-- >>> ErrorMessage <$> exampleMessage <*> exampleError
--Just (ErrorMessage {message = "Opps", errorCode = 123})

-- This pattern works with any instance of Monad. In this case, you’re not working with values in a Maybe context but in a Parser context.

-- (.:) type signature
-- (.:) :: FromJSON a => Object -> Text -> Parser a
-- This operator takes an Object (your JSON object) and some text and returns a value parsed into a context.
-- so v .: "message" results in a value in a Parser context.  We need a context for your parse so that it can fail if there’s trouble parsing

--------------------
-- 5. parse JSON with customized FromJSON
sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError
-- *Main> sampleErrorMessage
-- Just (ErrorMessage {message = "oops!", errorCode = 123})
instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) =
         object [ "message" .= message
                , "error" .= errorCode
                ]

----------------------
-- 6. Create an error message to test your instance of ToJSON
anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0
-- *Main> encode anErrorMessage
-- "{\"error\":0,\"message\":\"Everything is Okay\"}"


-------------------------------------------------------
-- From unit7/ lesson 39 to print JSON. codes from unit7/lesson40/MainFromJSON.hs
data NOAAResult = NOAAResult
        { uid :: T.Text
        , mindate :: T.Text
        , maxdate :: T.Text
        , name :: T.Text
        , datacoverage :: Float
   --     , datacoverage :: Int (api has changed)
        , resultId :: T.Text
        } deriving Show



instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult <$> v .: "uid"
                   <*> v .: "mindate"
                   <*> v .: "maxdate"
                   <*> v .: "name"
                   <*> v .: "datacoverage"
                   <*> v .: "id"

data Resultset = Resultset
        { offset :: Int
        , count :: Int
        , limit :: Int
        } deriving (Show,Generic)

instance FromJSON Resultset

-- Metadata data type itself has only the Resultset value
newtype Metadata = Metadata
               {
                  resultset :: Resultset
               } deriving (Show,Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) =  do
            forM_ results (print . name)
            -- print name

printResults' :: Either String [NOAAResult] -> IO ()
printResults' (Left error) = print error
printResults' (Right results) = forM_ results (print . name)

-- book error
-- main :: IO ()
-- main = do
--     jsonData <- B.readFile "data.json"-- "../lesson39/http-lesson/data.json"
--     let noaaResponse = decode jsonData :: Maybe NOAAResponse -- change this
--     let noaaResults = results <$> noaaResponse
--     printResults noaaResults

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
--  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults' noaaResults