-- {-# LANGUAGE StandaloneDeriving #-}

-- QuickCheck 16.1
-- record syntax
-- data AuthorName = Author {
--       first_name :: String
--     , second_name :: String }

-- QuickCheck 16.2
-- data SportsCar = SportsCar Car Spoiler


type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName   
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName --for edge case e.g H.P. Lovecraft
          | FirstNameWithTwoInits FirstName Char Char -- more edge case

data Creator = AuthorCreator Author 
             | ArtistCreator Artist   
             
data Author = Author Name

data Artist = Person Name | Band String


data Book = Book {
     author    :: Creator   
   , isbn      :: String   
   , bookTitle :: String   
   , bookYear  :: Int   
   , bookPrice :: Double   } 

-- :set -XStandaloneDeriving
-- deriving instance Show Creator



data VinylRecord = VinylRecord {
         artist        :: Creator   
       , recordTitle   :: String   
       , recordYear    :: Int   
       , recordPrice   :: Double   }


data CollectibleToy = CollectibleToy {
         name :: String   
       , descrption :: String   
       , toyPrice :: Double   }

data StoreItem = BookItem Book 
               | RecordItem VinylRecord 
               | ToyItem CollectibleToy -- addition

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy


-- QuickCheck 16.3
madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"