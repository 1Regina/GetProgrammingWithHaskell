-- Product types. `And`
-- record syntax for Book
-- data Book = Book {
--        bkAuthor  :: AuthorName
--      , bkIsbn    :: String
--      , title   :: String
--      , year    :: Int
--      , price   :: Double}

-- QuickCheck 16.1
-- record syntax
-- data AuthorName = Author {
--       first_name :: String
--     , second_name :: String }

-- QuickCheck 16.2
-- data SportsCar = SportsCar Car Spoiler

-- bookstore inventory example
-- Sum type `Or`
type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName                      -- RHS Name is a type constructor
          | NameWithMiddle FirstName MiddleName LastName -- NameWithMiddle is a type constructor consisting of 3 strings
          | TwoInitialsWithLast Char Char LastName --for edge case e.g H.P. Lovecraft
          | FirstNameWithTwoInits FirstName Char Char -- more edge case
          deriving (Show)

data Creator = AuthorCreator Author
             | ArtistCreator Artist deriving (Show)

newtype Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

-- record syntax style
data Book = Book {
     author    :: Creator
   , isbn      :: String
   , bookTitle :: String
   , bookYear  :: Int
   , bookPrice :: Double   }

-- :set -XStandaloneDeriving
-- deriving instance Show Creator


-- record syntax style
data VinylRecord = VinylRecord {
         artist        :: Creator
       , recordTitle   :: String
       , recordYear    :: Int
       , recordPrice   :: Double   }


data CollectibleToy = CollectibleToy {
         name :: String
       , descrption :: String
       , toyPrice :: Double   }


-- Sum type
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


-- extra learning
title :: StoreItem -> String
title (BookItem book) = bookTitle book
title (RecordItem record) = recordTitle record
title (ToyItem toy) = name toy

-- Test
testBook :: Book
testBook = Book {
            author    = AuthorCreator ( Author ( Name "Test" "Ing") )  -- or simply author = hpLovecraft
          , isbn      = "123456abc"
          , bookTitle = "Test my codes"
          , bookYear  = 2021
          , bookPrice = 9.99

        }

-- >>> madeBy (BookItem testBook)
-- "AuthorCreator (Author (Name \"Test\" \"Ing\"))"
-- >>> price (BookItem testBook)
-- 9.99
-- >>> title (BookItem testBook)
-- "Test my codes"


hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                           (Author
                              (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

testBookHP :: Book
testBookHP = Book {
            author    = hpLovecraft
          , isbn      = "123456abc"
          , bookTitle = "Test my codes"
          , bookYear  = 2021
          , bookPrice = 9.99

        }


-- >>> madeBy (BookItem testBookHP)
-- "AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' \"Lovecraft\"))"


-- >>> price  (BookItem testBookHP)
-- 9.99

