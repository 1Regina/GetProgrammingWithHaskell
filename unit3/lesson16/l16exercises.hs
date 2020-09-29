-- Q16.1
type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName   
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName --for edge case e.g H.P. Lovecraft
          | FirstNameWithTwoInits FirstName Char Char -- more edge case
          deriving Show

data Creator = AuthorCreator Author 
             | ArtistCreator Artist  
             deriving Show
             
data Author = Author Name deriving Show

data Artist = Person Name | Band String deriving Show


data Book = Book {
     author    :: Creator   
   , isbn      :: String   
   , bookTitle :: String   
   , bookYear  :: Int   
   , bookPrice :: Double   } 


data VinylRecord = VinylRecord {
         artist        :: Creator   
       , recordTitle   :: String   
       , recordYear    :: Int   
       , recordPrice   :: Double   }


data CollectibleToy = CollectibleToy {
         name       :: String   
       , descrption :: String   
       , toyPrice   :: Double   }

data Pamphlet = Pamphlet {
         title          :: String
       , pamphletDesc   :: String
       , contact        :: Int
    --    , pamphletPrice  :: Double
    }

data StoreItem = BookItem Book 
               | RecordItem VinylRecord 
               | ToyItem CollectibleToy -- addition
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem     book    ) = show $ author book
madeBy (RecordItem   record  ) = show $ artist record
madeBy (PamphletItem pamphlet) = show $ contact pamphlet
madeBy (ToyItem      toy     ) = "unknown"

-- Q16.2
--recall patient name or author name
data Shape = Circle Radius | Square Length | Rectange Length Breadth --deriving Show
type Radius = Double
-- data Circle = CircleArea Radius
--             | CirclePeri Radius

-- data Square = SquareArea Length
--             | SquarePeri Length
type Length = Double
type Breadth = Double
-- data Rectange = RectArea Length Breadth
--               | RectPeri

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square side) = 4 * side
perimeter (Rectange l b) = 2 * (l + b)

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square side) = side ^ 2
area ( Rectange l b) = l * b
