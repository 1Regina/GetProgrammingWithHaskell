--Q14.1
data Digit = One | Two | Three deriving (Enum)

--Use fromEnum to get to Int which have instances of the Eq and Ord type classes
instance Eq Digit where
-- Minimal complete definition for Eq is (==) | (/=)
    (==) digit1 digit2 = (fromEnum digit1) == (fromEnum digit2)

instance Ord Digit where
-- Minimal complete definition for Ord is compare | (<=)
    compare digit1 digit2 = compare (fromEnum digit1) (fromEnum digit2)

--Q14.2 (Ans)
-- qn : define a five-sided die
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show) -- Dont need Ord as Eq is a superclass of Ord

-- qn: define a type class named Die with a useful method for a die. Include a superclass (chose Eq as superclass of Ord which die must have)
class (Eq a, Enum a) => Die a where
 roll :: Int ->  a

-- qn: make your FiveSidedDie an instance of Die
instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)