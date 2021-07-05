import Data.Char (toLower)
import qualified Data.Map as Map

-- an abstract container that can hold any other type. Once a concrete value is inside the box, the box type = the concrete value type.

data Box a = Box a deriving Show

-- first Box = Type constructor
-- first a = declaration of a type variable
-- second Box = Data constructor
-- second a = Type variable being used

-- See how Box a type change according to different types of a
n = 6 :: Int
word = "box"
f x = x
otherBox = Box n

-- >>> :t Box n
-- >>> :t Box word
-- >>> :t Box f
-- >>> :t Box otherBox
-- Box n :: Box Int
-- Box word :: Box [Char]
-- Box f :: forall p. Box (p -> p)
-- Box otherBox :: Box (Box Int)

char = 'a'
-- >>> :t Box char
-- Box char :: Box Char


wrap :: a -> Box a
wrap x = Box x
unwrap :: Box a -> a
unwrap (Box x) = x

-- >>> :t wrap
-- wrap :: forall a. a -> Box a
-- >>> :t wrap n
-- >>> :t wrap word
-- >>> :t wrap f
-- >>> :t wrap otherBox
-- >>> :t wrap char
-- wrap n :: Box Int
-- wrap word :: Box [Char]
-- wrap f :: forall p. Box (p -> p)
-- wrap otherBox :: Box (Box Int)
-- wrap char :: Box Char

-- >>> :t unwrap
-- unwrap :: forall a. Box a -> a
-- >>> :t unwrap (Box n)
-- >>> :t unwrap (Box word)
-- >>> :t unwrap (Box f)
-- >>> :t unwrap (Box otherBox)
-- >>> :t unwrap (Box char)
-- unwrap (Box n) :: Int
-- unwrap (Box word) :: [Char]
-- unwrap (Box f) :: forall p. p -> p
-- unwrap (Box otherBox) :: Box Int
-- unwrap (Box char) :: Char

--Quickcheck 18.1
-- wrap :: a -> Box a
-- >>> :t wrap (Box char)
-- wrap (Box char) :: Box (Box Char)

-- Triple Tuple with same type
-- exammple 1: a point in space
data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- exammple 2: a full name
type FullName = Triple String
aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- example 3: triple initial
type Initials = Triple Char
initials :: Initials
initials = Triple 'H' 'P' 'L'

-- Other Tuple functions
toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- >>> transform (* 3) aPoint
-- >>> transform reverse aPerson
-- >>> transform toLower initials
-- >>> toList (transform toLower initials)
-- Triple 0.30000000000000004 159.60000000000002 36.900000000000006
-- Triple "drawoH" "spillihP" "tfarcevoL"
-- Triple 'h' 'p' 'l'
-- "hpl"


-- Quick check 18.2 `transform` vs `map`
-- >>> :t transform
-- >>> :t map
-- transform :: forall a. (a -> a) -> Triple a -> Triple a
-- map :: forall a b. (a -> b) -> [a] -> [b]

-- The transform function doesn’t allow you to change the type; that is, a function (a -> b). The map function for lists does allow this

-- List comparison mylist vs built-in list
data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]
ourListEx1' :: List Int
ourListEx1' = Cons 1 (Cons 2 (Cons 3 Empty))
builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]
ourListEx2' :: List Char
ourListEx2' = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- >>> builtinEx1
-- [1,2,3]
-- >>> builtinEx2
-- "cat"

-- mapping a list
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest)  = Cons (func a) (ourMap func rest)
-- >>> ourMap (*2) ourListEx1'
-- Cons 2 (Cons 4 (Cons 6 Empty))


-- List of tuples e.g inventory with (item, price) using (String, Int)
itemCount1 :: (String,Int)
itemCount1 = ("Erasers",25)
itemCount2 :: (String,Int)
itemCount2 = ("Pencils",25)
itemCount3 :: (String,Int)
itemCount3 = ("Pens",13)

itemInventory :: [(String,Int)]
itemInventory = [itemCount1,itemCount2,itemCount3]

-- >>> itemInventory
-- [("Erasers",25),("Pencils",25),("Pens",13)]

-- Quick check 18.3
-- adding ("Paper",12.4) will fail as 12.4 :: Double , not Int

-- Kinds
-- >>> :kind Int
-- >>> :kind Triple
-- >>> :kind []
-- >>> :kind (,)
-- >>> :kind [Int]
-- >>> :kind Triple Char
-- Int :: *
-- Triple :: * -> *
-- [] :: * -> *
-- (,) :: * -> * -> *
-- [Int] :: *
-- Triple Char :: *


-- Quick check 18.4
-- >>> :kind (, ,)
-- (, ,) :: * -> * -> * -> *

-- use of Map.Map . Rem to import qualified Data.Map as Map
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- why fromList:
-- fromList :: Ord k => [(k,a)] -> Map k a

-- to retrieve value in Map (aka Dictionary). Note the Maybe output
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

-- >>> Map.lookup 7 organCatalog
-- Just Heart

