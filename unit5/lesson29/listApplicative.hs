-- Quick check 29.1    Use    <$> and <*> to combine two Maybe String types with ++.
-- >>> (++) <$> Just "Haskell" <*> Just " Day"
-- Just "Haskell Day"

-- 1. Put value in a context:  For `Maybe`, `pure` gives a `Just`
-- >>> pure 6 :: Maybe Int
-- Just 6
-- Put function in a context with pure
-- >>> (6+) <$> Just 5
-- Just 11
-- >>> pure (6+) <*> Just 5
-- Just 11
-- >>> (+6) <$> Just 5
-- Just 11
-- >>> pure (+6) <*> Just 5
-- Just 11

-- Quick check 29.2    Make    the    String "Hello World" into an IO String
hello :: IO String
hello = pure "Hello World"
-- >>> hello
-- "Hello World"

--Quick check 29.3    Suppose you want to make it so that (pure +) <*> (1,2) <*> (3,4) =(1+2,1+4,2+3,2+4) = (3,5,5,6). Why doesn’t this work?
-- (pure +) <*> (1,2) <*> (3,4) =(1+2,1+4,2+3,2+4) = (3,5,5,6)
-- QC 29.3 answer This  doesn’t  work  because  (3,5,5,6)  is  an  entirely  different  type  than  (1,2)  or(3,4). The first is type (a,b,c,d), and the other two are (a,b)
-- >>> (pure +) <*> (1,2) <*> (3,4)
-- Couldn't match expected type ‘(a, Integer -> Integer -> b)’
--             with actual type ‘(a0 -> f0 a0) -> a0 -> f0 a0’
-- >>> (1+2,1+4,2+3,2+4)
-- (3,5,5,6)

-- For list which is both a container and a context, Context question for a list: to use Applicative, you need to be able to answer the question, “What does it mean to apply a function to two or more values in the context of a list?”
-- >>> pure (+) <*> [1000,2000,3000] <*> [500,20000]
-- [1500,21000,2500,22000,3500,23000]

-- A Game Show. 3 doors + 2 boxes
-- 1. Nondeterministic possibilities for door values
doorPrize :: [Int]
doorPrize = [1000,2000,3000]
-- 2. Nondeterministic possibilities for box prizes
boxPrize :: [Int]
boxPrize = [500,20000]
-- 3. deterministic context => 1 door + 1 box
-- non-deterministic context => all possible combinations of doors + boxes
-- deterministic = 1 final prize path
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize
-- totalPrize represents all possible prizes that can be won: from unit5/lesson29/listAsContext.png
-- >>> totalPrize
-- [1500,21000,2500,22000,3500,23000]

-- Quick check 29.4    Solve this problem if the boxes contain a prize multiplier instead of just anadditional prize. The multipliers are 10× and 50×.
boxPrizeMultiplier :: [Int]
boxPrizeMultiplier = [10,50]
-- >>> pure (*) <*>  doorPrize <*> boxPrizeMultiplier
-- [10000,50000,20000,100000,30000,150000]
-- >>> pure (*) <*> [1000,2000,3000] <*> [10,50]
-- [10000,50000,20000,100000,30000,150000]
newPrizes = pure (*) <*>  doorPrize <*> boxPrizeMultiplier
-- >>> newPrizes
-- [10000,50000,20000,100000,30000,150000]


-- A. Find prime numbers
-- 1. understand composites
-- pure (*) <*> [2 .. 4] <*> [2 .. 4]
-- [4,6,8,6,9,12,8,12,16]

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isNotComposite = not . (`elem` composite)
-- >>> primesToN 32
-- [2,3,5,7,11,13,17,19,23,29,31]

--B. Create data in IO and Maybe previously, now do so with Applicative
data User = User {
     name :: String
   , gamerId :: Int
   , score :: Int
   } deriving Show
-- 1. testNames
testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"]
testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]


testData :: [User]
testData = pure User <*> testNames
                     <*> testIds
                     <*> testScores

-- >>> length testData
-- 36
-- >>> take 2 testData
-- [User {name = "John Smith", gamerId = 1337, score = 0},User {name = "John Smith", gamerId = 1337, score = 100000}]

-- Quick  check  29.5    Add    your    own    name    to    testNames  and  regenerate  the  data.  How  manyexamples are there now?
testNamesAdd :: [String]
testNamesAdd = ["Haskell Girl"
               ,"John Smith"
               ,"Robert'); DROP TABLE Students;--"
               ,"Christina NULL"
               ,"Randall Munroe"]

testData1 :: [User]
testData1 = pure User <*> testNamesAdd
                      <*> testIds
                      <*> testScores

-- >>> length testData1
-- 45
