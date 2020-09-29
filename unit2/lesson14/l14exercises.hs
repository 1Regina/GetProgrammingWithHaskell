--Q14.1
data Digit = One | Two | Three deriving (Enum)

--Use fromEnum to get to Int which have instances of the Eq and Ord type classes
instance Eq Digit where 
-- Minimal complete definition for Eq (==) | (/=)
    (==) digit1 digit2 = (fromEnum digit1) == (fromEnum digit2)

instance Ord Digit where
-- Minimal complete definition for Ord compare | (<=)
    compare digit1 digit2 = compare (fromEnum digit1) (fromEnum digit2)

--Q14.2 (Ans)
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show) -- Dont need Ord as Eq is a superclass of Ord

class (Eq a, Enum a) => Die a where
 roll :: Int ->  a
instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)