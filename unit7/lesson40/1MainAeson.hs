module 1MainAeson where
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

--Quick check 40.1     Why does encode return a ByteString rather than a MaybeByteString?
-- encode :: ToJSON a => a -> ByteString ; takes a type that’s an instance of `ToJSON` and returns a JSON object represented as a ByteString so it would only return a JSON. Concern is whether it may not be able to parsed for the original type.

-- Quick check 40.2  Use Generic to implement ToJSON and FromJSON for this type:
data Name = Name
        { firstName :: T.Text
        , lastName :: T.Text    } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

-- Quick check 40.3    Make the Name type into an instance of FromJSON without Generic:
-- Quick check 40.4     Finally, make Name an instance of ToJSON without Generic:
data Name = Name
        { firstName :: T.Text
        , lastName :: T.Text
        } deriving (Show)

-- Quick check 40.3
instance FromJSON Name where
        parseJSON (Object v) =
                Name <$> v .: "firstname"
                     <*> v .: "lastname"

-- Quick check 40.4
instance ToJSON Name where
        toJSON (Name firstName lastName) =
                object [  "firstname" .= firstname
                        , "lastname" .= lastname
                        ]

-- Q40.2    Make    a    Sum type called IntList and use DerivingGeneric to make it an instance of ToJSON. Don’t use the existing List type, but rather write it from scratch. Here’s an example of an IntList:

intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList

data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)
instance ToJSON IntList
instance FromJSON IntList