import qualified Data.Map as Map -- for 5B The Maybe context—working with a map of candidates

-- 1. review and cultural fit with Grade data type
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
-- 2. Degree data type for highest level of education
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
-- 3.  Candidate data type representing performance on an interview
data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

-- 4. viable function checks whether your Candidate qualify to proceed for committee review
viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [passedCoding,passedCultureFit,educationMin]

-- Quick check 31.2    Create    a    Candidate type and see whether that candidate is viable
candidateSample :: Candidate
candidateSample = Candidate
    { candidateId = 001
    , codeReview = A
    , cultureFit = A
    , education  = PhD  }

-- >>> viable candidateSample
-- True

-- 5. Different Contexts
-- Useful IO actions for building your Candidate with helper functions
readInt :: IO Int
readInt = getLine >>= (return . read)
readGrade :: IO Grade
readGrade = getLine >>= (return . read)
readDegree :: IO Degree
readDegree = getLine >>= (return . read)

-- 5A IO context
readCandidateIO :: IO Candidate
readCandidateIO = do
    putStrLn "Record candidate results"
    putStrLn "Enter candidate ID: "
    cID <- readInt
    putStrLn "Enter code review grade: "
    codeGrade <- readGrade
    putStrLn "Enter cultural fit grade: "
    cultureGrade <- readGrade
    putStrLn "Enter highest education level: "
    educationLevel <- readDegree
    return (Candidate {candidateId = cID
                     , codeReview  = codeGrade
                     , cultureFit  = cultureGrade
                     , education   = educationLevel })

-- 5A.1 An IO action that lets you know if candidate passed/failed
assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidateIO
    let passed = viable candidate  -- where all tests == True
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- Quick check 31.3 Rewrite readGrade with do-notation.
readGradeDo :: IO Grade
readGradeDo = do
    gradeInput <- getLine
    return (read gradeInput)


-- 5B The Maybe context—working with a map of candidates.
-- 5B.1 Create some candidates
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

-- 5B.2  Create a candidate DB
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                           ,(2,candidate2)
                           ,(3,candidate3)]

-- 5B.3 Because each lookup will return a Maybe type, you have a problem in a different context than the IO case before.
-- need assessCandidateIO cousin for Maybe assessCandidateMaybe
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cID = do
    candidate <- Map.lookup cID candidateDB
    let passed = viable candidate  -- where all tests == True
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- >>> assessCandidateMaybe 2
-- Just "failed"
-- >>> assessCandidateMaybe 3
-- Just "passed"
-- >>> assessCandidateMaybe 5
-- Nothing

-- 5 COMPARISON
-- assessCandidateIO  and assessCandidateMaybe and assessCandidateList are almost identical.  because after you assign a variable with <- in do-notation, you get to pretend it’s an ordinary type that’s not in a particular context. The Monad type class and do-notation have abstracted away the context you’re working in.

-- Quick  check  31.4    Write    a    simple    function    Maybe  String  ->  String  that  will  printfailed/passed if there’s a result and error id not found for the Nothing constructor.

failPassElse :: Maybe  String  ->  String
failPassElse Nothing = "error id not found"
failPassElse (Just result) = result

-- 5C. The List context
-- 5C.1 Possible Candidates in a list context
candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

-- 5C.2 Assessing a list of candidates using List as a MonadListing
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
        candidate <- candidates
        let passed = viable candidate  -- where all tests == True
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement
-- >>>  assessCandidateList candidates
-- ["failed","failed","passed"]

-- Quick check 31.5    Does    assessCandidateList handle the empty list
candidatesEmpty :: [Candidate]
candidatesEmpty = []

assessCandidateListEmpty :: [Candidate] -> [String]
assessCandidateListEmpty  candidates = do
        candidate <- candidatesEmpty
        let passed = viable candidate  -- where all tests == True
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement
-- >>>  assessCandidateListEmpty candidates
-- []

-- It does! Passing any empty list to assessCandidateList returns the empty list

-- 5C.3 inferior code for list with map which cant generalise to other context
assessCandidateListBad :: [Candidate] -> [String]
assessCandidateListBad candidates = map (\x -> if x
                                        then "passed"
                                        else "failed") passed
       where passed = map viable candidates

-- 5D A universal assessCandidate function with simply a common type class constraint.
assessCandidate :: Monad m =>  m Candidate -> m String
assessCandidate candidates = do
        candidate <- candidates
        let passed = viable candidate  -- where all tests == True
        let statement = if passed
                            then "passed"
                            else "failed"
        return statement

-- 5D.1 Test in IO context by `ghci 2monad_do_3Contexts.hs`
-- *Main> assessCandidate readCandidateIO
-- Record candidate results
-- Enter candidate ID:
-- 8
-- Enter code review grade:
-- A
-- Enter cultural fit grade:
-- A
-- Enter highest education level:
-- MS
-- "passed"

--5D.2 Test in Maybe context
-- >>> assessCandidate (Map.lookup 1 candidateDB)
-- Just "failed"
-- >>> assessCandidate (Map.lookup 2 candidateDB)
-- Just "failed"
-- >>> assessCandidate (Map.lookup 3 candidateDB)
-- Just "passed"
-- >>> assessCandidate candidates
-- ["failed","failed","passed"]
